#' Pulse Helper: Calculating Heart Rate
#'
#' A helper function to calculate heart rate
#'
#' @param signal an integer vector, infrared signal from pulse device.
#' @param sampling.rate an integer, sampling rate in Hz.
#' @param cor.min a numeric, the correlation threshold for qualified signal.
#' @param score.parameter a numeric, the exponent used in the score.
#' @param display a character string, whether to plot display.
#' @param score.method a character string, a method for lag penalty.
#' @param upsampling.factor
#'
#' @returns a mixed list, \code{locmax} for local maxima, \code{nominee} for the best peak, \code{cor.pass} and \code{anticor.pass} for quality control, and \code{hr} for final heart rate.
#' @export
#'
#' @seealso [pulse.extract()]
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
pulse.hr <- function(signal,
                     sampling.rate = 5, upsampling.factor = 10,
                     score.method = c("power.law", "exponential"), score.parameter = 0.5,
                     cor.min = 0.5,
                     display = c("all", "ppg", "none")
){
  signal.length <- length(signal)

  # upsampling of signal (e.g., from 5 Hz to 50 Hz - ensure 1 bpm resolution bw 2-60 bpm)
  upsignal.length <- signal.length * upsampling.factor
  upsignal <- approx(x = 1 : signal.length, y = signal, n = upsignal.length)$y

  upsampling.rate <- sampling.rate * upsampling.factor

  # autocorrelation
  lag.max <- upsignal.length / 2 # half of the upsignal length
  # can use NROW instead of length() to suit 1 column matrix or dataframe
  # lag.max <- 1/2 * 60 * sampling.rate # e.g., No. data points in half a minute (min detectable hr = 2 bpm)
  timelag.max <- lag.max / upsampling.rate

  ac.list <- acf(upsignal,
                 lag.max = lag.max,
                 plot = FALSE # will plot manually if needed
  )

  ac <- data.frame(lag = ac.list$lag,
                   cor = ac.list$acf) # note that cor = 1 at lag = 0

  # find all local maxima (peaks)
  locmax.idx <- which(diff(sign(diff(ac$cor))) == -2) + 1
  locmax <- ac[locmax.idx, ]

  # convert lag to time in seconds
  locmax$timelag <- locmax$lag / upsampling.rate

  # compute score using power law decay or exponential decay
  score.method <- match.arg(score.method)

  if (score.method == "power.law") {
    locmax$score <- locmax$cor / (locmax$timelag)^score.parameter
  } else if (score.method == "exponential") {
    locmax$score <- locmax$cor * exp(- score.parameter * locmax$timelag)
  }

  # ifelse(locmax$cor <= 0, # only calculate score for positive cor (actually not necessary, negative score anyway)
  #                      NA,
  #                      (locmax$cor)^score.exponents[1] / (locmax$lag)^score.exponents[2]
  #                      )

  # compute hr
  locmax$hr <- 60 / locmax$timelag # beats per minute (bpm)

  # best candidate
  nominee <- locmax[which.max(locmax$score), ]

  # explicit fix the case of locmax cor all negative, score all NA, which.max returns empty (no longer needed)
  # nominee <- locmax[which.max(locmax$score)[1], ] #alternative trick, adding [1], will return NA df

  if(nrow(nominee) == 0) { # just in case nominee is empty
    nominee <- locmax[NA_integer_, ]
    cor.pass <- anticor.pass <- hr <- NA
  } else {
    # quality check for cor
    cor.pass <- nominee$cor >= cor.min

    # quality check for anti-correlation
    # trivial rhythms, no negative correlation occur before the dominant peak
    segment <- ac[ac$lag < nominee$lag, ] # segment preceding the dominant peak
    # segment <- subset(ac, lag < nominee$lag) #  subset() causes no visible binding for global variable issue
    anticor.pass <- any(segment$cor < 0)

    # final hr
    if (cor.pass && anticor.pass) {
      hr <- nominee$hr
    }
    else hr <- NA
  }

  quality <- c(cor = cor.pass, anticor = anticor.pass)

  # output list
  output <- list(
    # ac.list = ac.list,
    # ac = ac,
    locmax = locmax,
    nominee = nominee,
    quality = quality,
    hr = hr)

  # display
  display <- match.arg(display)

  if (display %in% c("all", "ppg")) {
    plot(upsignal, type = "l", main = "Photoplethysmogram", ylab = "IR signal",
         ylim = c(0, 4095)
         )
    text(upsignal.length, 0, labels = paste(upsignal.length / upsampling.rate, "s\n", upsampling.rate, "Hz"),
         adj = c(1, 0), col = 2)
  }

  if (display == "all") { # hide these if human counting
    plot(ac.list, main = "Autocorrelogram", ci = 0)

    plot(locmax$timelag, locmax$cor, type = "o", pch = 19, lty = 5,
         main = "Local maxima", xlab = "Time lag (s)", ylab = "",
         xlim = c(0, timelag.max), ylim = c(0, 1))
    lines(locmax$timelag, locmax$score, type = "o", pch = 19, lty = 5, col = 2)
    points(nominee$timelag, nominee$score, col = 2, cex = 3)
    text(nominee$timelag, nominee$score, labels = paste(round(nominee$hr, 1), "bpm"), #"HR =",
         pos = 3, offset = 1, col = 2)
    abline(h = cor.min, col = 4, lty = 2)
    # abline(v = nominee$timelag, col = 3, lty = 2)
    segments(x0 = nominee$timelag, y0 = -0.1, x1 = nominee$timelag, y1 = nominee$score, col = 3, lty = 2)
    legend("topright", legend = c("Correlation", "Score"),
           col = 1:2, text.col = 1:2, pch = 19, lty = 5)

    print(output)
  }

  output
}
