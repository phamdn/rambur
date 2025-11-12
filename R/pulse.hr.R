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
pulse.hr <- function(signal, sampling.rate = 5, cor.threshold = 0.7, ratio.thredshold = 0.95){

  # autocorrelation
  ac.list <- acf(signal,
                 lag.max = min(c(NROW(signal) / 2, 1 * 60 * sampling.rate)),
                 # half of the signal length but no more than 1 minute (min detectable hr = 1 bpm)
                 # use NROW instead of length() to suit 1 column matrix or dataframe
                 plot = FALSE
                 )

  ac <- data.frame(lag = ac.list$lag,
                   cor = ac.list$acf) # note that cor = 1 at lag = 0

  # find all local maxima (peaks)
  locmax.idx <- which(diff(sign(diff(ac$cor))) == -2) + 1
  locmax <- ac[locmax.idx, ]

  # filter out qualified peaks using threshold
  qualified <- subset(locmax, cor >= cor.threshold)
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
      if (all(ratio < ratio.thredshold)) { # heuristic threshold
        dominant <- qualified[highest.idx, ]
      } else { # similar in strength, choose the earliest
        dominant <- qualified[which(ratio >= ratio.thredshold)[1], ]
      }
    }
  }

  # check for trivial rhythms
  if (qualified.count >= 1) {
    segment <- subset(ac, lag < dominant$lag) # segment preceding the dominant peak
    if (all(segment$cor >= 0)) { # in case no negative correlation occur before the dominant peak
      dominant[1, ] <- c(NA, NA)
    }
  }

  # calculate heart rate
  hr <- 60 / (dominant$lag / sampling.rate)

  # results as a list
  list(ac.list = ac.list,
       # ac = ac,
       locmax = locmax,
       qualified = qualified,
       dominant = dominant,
       hr = hr)
}
