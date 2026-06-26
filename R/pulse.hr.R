#' Pulse Helper: Calculating Heart Rate
#'
#' A helper function to calculate heart rate
#'
#' @param signal an integer vector, infrared signal from pulse device.
#' @param sampling.rate an integer, sampling rate in Hz.
#' @param cor.min a numeric, the correlation threshold for qualified signal.
#' @param score.parameter a numeric, the exponent used in the score.
#' @param diagnostics a character string, whether to plot diagnostics.
#' @param score.method a character string, a method for lag penalty.
#'
#' @returns a mixed list, \code{locmax} for local maxima, \code{highest.score} for the best peak, \code{cor.pass} and \code{anticor.pass} for quality control, and \code{hr} for final heart rate.
#' @export
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' ex1 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:01:00" &
#' datetime <= "2025-05-21 00:02:00", channel.1, drop = TRUE))
#' ex2 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 01:07:00" &
#' datetime <= "2025-05-21 01:08:00", channel.1, drop = TRUE))
#' ex3 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:09:00" &
#' datetime <= "2025-05-21 00:10:00", channel.3, drop = TRUE))
#' ex4 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:30:00" &
#' datetime <= "2025-05-21 00:31:00", channel.10, drop = TRUE))
pulse.hr <- function(signal, sampling.rate = 5,
                     score.method = "power.law", score.parameter = 1,
                     cor.min = 0.4,
                     diagnostics = c("all", "ppg", "none")
){

  # autocorrelation
  lag.max <- NROW(signal) / 2 # half of the signal length
  # use NROW instead of length() to suit 1 column matrix or dataframe
  # lag.max <- 1/2 * 60 * sampling.rate # e.g., No. data points in half a minute (min detectable hr = 2 bpm)

  ac.list <- acf(signal,
                 lag.max = lag.max,
                 plot = FALSE # will plot manually if diagnostics are needed
  )

  ac <- data.frame(lag = ac.list$lag,
                   cor = ac.list$acf) # note that cor = 1 at lag = 0

  # find all local maxima (peaks)
  locmax.idx <- which(diff(sign(diff(ac$cor))) == -2) + 1
  locmax <- ac[locmax.idx, ]

  # compute score using power law decay, will consider exponential decay in future
  # score.method <- match.arg(score.method)

  if (score.method == "power.law") {
    locmax$score <- locmax$cor / (locmax$lag)^score.parameter
  }
  # ifelse(locmax$cor <= 0, # only calculate score for positive cor (actually not necessary, negative score anyway)
  #                      NA,
  #                      (locmax$cor)^score.exponents[1] / (locmax$lag)^score.exponents[2]
  #                      )

  # compute hr
  locmax$hr <- 60 / (locmax$lag / sampling.rate) # beats per minute (bpm)

  # best candidate
  highest.score <- locmax[which.max(locmax$score), ]

  # explicit fix the case of locmax cor all negative, score all NA, which.max returns empty (no longer needed)
  # highest.score <- locmax[which.max(locmax$score)[1], ] #alternative trick, adding [1], will return NA df

  if(nrow(highest.score) == 0) { # just in case highest.score is empty
    highest.score <- locmax[NA_integer_, ]
    cor.pass <- anticor.pass <- hr <- NA
  } else {
    # quality check for cor
    cor.pass <- highest.score$cor >= cor.min

    # quality check for anti-correlation
    # trivial rhythms, no negative correlation occur before the dominant peak
    segment <- ac[ac$lag < highest.score$lag, ] # segment preceding the dominant peak
    # segment <- subset(ac, lag < highest.score$lag) #  subset() causes no visible binding for global variable issue
    anticor.pass <- any(segment$cor < 0)

    # final hr
    if (cor.pass && anticor.pass) {
      hr <- highest.score$hr
    }
    else hr <- NA
  }

  # output list
  output <- list(
    # ac.list = ac.list,
    # ac = ac,
    locmax = locmax,
    highest.score = highest.score,
    cor.pass = cor.pass,
    anticor.pass = anticor.pass,
    hr = hr)

  # diagnostics
  diagnostics <- match.arg(diagnostics)

  if (diagnostics %in% c("all", "ppg")) {
    plot(signal, type = "l", main = "Photoplethysmogram", ylab = "IR signal")
  }

  if (diagnostics == "all") { # hide these if human counting
    plot(ac.list, main = "Autocorrelogram")

    plot(locmax$lag, locmax$cor, type = "o",
         main = "Local maxima", xlab = "Lag", ylab = "Correlation",
         xlim = c(0, lag.max), ylim = c(0, 1))
    points(highest.score$lag, highest.score$cor, col = 2, cex = 3)
    text(highest.score$lag, highest.score$cor, labels = paste("HR =", round(highest.score$hr, 1), "bpm"),
         pos = 3, offset = 1, col = 2)
    abline(h = cor.min, col = 4, lty = 2)

    print(output)
  }

  output
}
