#' Pulse Helper: Calculating Heart Rate
#'
#' @param signal
#' @param sampling.rate
#' @param cor.threshold
#'
#' @returns
#' @export
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:01:00" & datetime <= "2025-05-21 00:02:00", channel.1, drop = TRUE))
#' pulse.hr(subset(pulse.data, datetime >= "2025-05-21 01:07:00" & datetime <= "2025-05-21 01:08:00", channel.1, drop = TRUE))
#' pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:30:00" & datetime <= "2025-05-21 00:31:00", channel.10, drop = TRUE))
#'
pulse.hr <- function(signal, sampling.rate = 5,
                     score.exponents = c(2, 1), cor.threshold = 0.4,
                     diagnostics = TRUE
                     ){

  # autocorrelation
  lag.max <- 1/2 * 60 * sampling.rate # No. data points in half a minute (min detectable hr = 2 bpm)

  ac.list <- acf(signal,
                 lag.max = lag.max,
                 # lag.max = min(c(NROW(signal) / 2, 1 * 60 * sampling.rate)),
                 # half of the signal length but no more than 1 minute (min detectable hr = 1 bpm)
                 # use NROW instead of length() to suit 1 column matrix or dataframe
                 plot = FALSE # will plot manually below if needed
                 )

  ac <- data.frame(lag = ac.list$lag,
                   cor = ac.list$acf) # note that cor = 1 at lag = 0

  # find all local maxima (peaks)
  locmax.idx <- which(diff(sign(diff(ac$cor))) == -2) + 1
  locmax <- ac[locmax.idx, ]

  # compute score and hr
  locmax$score <- ifelse(locmax$cor <= 0, # only calculate score for positive cor
                         NA,
                         (locmax$cor)^score.exponents[1] / (locmax$lag)^score.exponents[2]
                         )

  locmax$hr <- 60 / (locmax$lag / sampling.rate) #beats per minute

  # best candidate
  highest.score <- locmax[which.max(locmax$score), ]

  # quality check for cor
  qualified.cor <- highest.score$cor >= cor.threshold

  # quality check for lag
  # trivial rhythms: no negative correlation occur before the dominant peak
  segment <- subset(ac, lag < highest.score$lag) # segment preceding the dominant peak
  qualified.lag <- any(segment$cor < 0)

  # final hr
  if (qualified.cor & qualified.lag) {
    hr <- highest.score$hr
  }
  else hr <- NA

  # output list
  output <- list(
    # ac.list = ac.list,
    # ac = ac,
    locmax = locmax,
    highest.score = highest.score,
    qualified.cor = qualified.cor,
    qualified.lag = qualified.lag,
    hr = hr)

  # diagnostics
  if (diagnostics) {
    plot(signal, type = "l", main = "Photoplethysmogram", ylab = "IR signal")
    plot(ac.list, main = "Autocorrelogram")
    plot(locmax$lag, locmax$cor, type = "o",
         main = "Local maxima", xlab = "Lag", ylab = "Pearson correlation",
         xlim = c(0, lag.max), ylim = c(0, 1))
    points(highest.score$lag, highest.score$cor, col = 2, cex = 3)
    text(highest.score$lag, highest.score$cor, labels = paste("HR =", round(highest.score$hr, 1), "bpm"),
         pos = 3, offset = 1, col = 2)
    abline(h = cor.threshold, col = 4, lty = 2)
    # abline(h = 0, col = 2, lty = 2)
    print(output)
  }

  output
}
