#' Pulse: Calculating Heart Rate
#'
#' @param signal
#' @param sampling.rate
#' @param threshold
#'
#' @returns
#' @export
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' folder <- system.file("extdata/pulse", package = "rambur")
#' rs <- pulse.read(folder)
#' enhanced.rs <- rs %>% transmute(datetime = floor_date(datetime, "minute"), across(channel.1:channel.10))
#' hrrs <- enhanced.rs %>% group_by(datetime) %>% summarize( hr.1 = hr.calc(channel.1)$hr, quality = hr.calc(channel.1)$cor)
#' plot(hrrs$hr.1)
#'
hr.calc <- function(signal, sampling.rate = 5, threshold = 0.7){

  # autocorrelation
  ac.list <- acf(signal,
                 lag.max = 1 * 60 * sampling.rate # number of data points in 1 minute
                 )

  ac <- data.frame(lag = ac.list$lag,
                   cor = ac.list$acf) # note that cor = 1 at lag = 0

  # find all local maxima (peaks)
  locmax.idx <- which(diff(sign(diff(ac$cor))) == -2) + 1
  locmax <- ac[locmax.idx, ]

  # filter out qualified peaks using threshold
  qualified <- subset(locmax, cor >= threshold)
  qualified.count <- nrow(qualified)

  # find "the" dominant peak
  if (qualified.count == 0) {
    dominant <- qualified
    dominant[1, ] <- c(NA, NA)
  }

  if (qualified.count == 1) {
    dominant <- qualified
  }

  if (qualified.count > 1) {
    highest.idx <- which.max(qualified$cor) # which.max always returns one index despite multiple equal maxs
    if (highest.idx == 1) {
      dominant <- qualified[1, ]
    } else {
      ratio <- qualified$cor[1:(highest.idx - 1)] / max(qualified$cor)
      if (all(ratio < 0.95)) { # 0.95 as heuristic threshold
        dominant <- qualified[highest.idx, ]
      } else { # similar in strength, choose the earliest
        dominant <- qualified[which(ratio >= 0.95)[1], ]
      }
    }
  }

  # check for trivial rhythms
  if (qualified.count >= 1) {
    segment <- subset(ac, lag < dominant$lag) # segment preceding the dominant peak
    if (all(segment$cor >= 0)) { # in case no negative correlation occur
      dominant[1, ] <- c(NA, NA)
    }
  }

  # calculate heart rate
  dominant$hr <- 60 / (dominant$lag / sampling.rate)

  dominant
}
