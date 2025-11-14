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
pulse.hr <- function(signal, sampling.rate = 5, cor.threshold = 0.7){

  # autocorrelation
  ac.list <- acf(signal,
                 lag.max = 0.5 * 60 * sampling.rate, # half a minute (min detectable hr = 2 bpm)
                 ### archive
                 # lag.max = min(c(NROW(signal) / 2, 1 * 60 * sampling.rate)),
                 # half of the signal length but no more than 1 minute (min detectable hr = 1 bpm)
                 # use NROW instead of length() to suit 1 column matrix or dataframe
                 plot = FALSE
                 )

  ac <- data.frame(lag = ac.list$lag,
                   cor = ac.list$acf) # note that cor = 1 at lag = 0

  # find all local maxima (peaks)
  locmax.idx <- which(diff(sign(diff(ac$cor))) == -2) + 1
  locmax <- ac[locmax.idx, ]

  ### troubleshoot
  plot(locmax)
  lines(locmax)
  lines(subset(locmax, cor >= cor.threshold), col = "blue")
  abline(h = cor.threshold, col = "green", lty = 2)
  abline(h = 0, col = "red", lty = 2)
  ###

  # filter out qualified peaks using threshold
  qualified <- subset(locmax, cor >= cor.threshold)
  qualified.count <- nrow(qualified)

  ### troubleshoot
  # print(qualified.analysis)
  # print(mean(qualified.analysis == -1)*100)
  # print(diff(qualified.analysis))
  ###

  # find "the" dominant peak
  if (qualified.count == 0) {
    # dominant <- qualified
    # dominant[1, ] <- c(NA, NA)
    # dominant <- qualified[1, ] # will also return a row of NAs if count = 0
    dominant <- qualified[NA_integer_, ] # clearer way to return NAs
  }

  if (qualified.count == 1) {
    # dominant <- qualified[1, ] # same effect as the below
    dominant <- qualified
  }

  # if (qualified.count <= 1) { # combine both cases of 0 and 1
  #   dominant <- qualified[1, ]
  # }

  if (qualified.count >= 2) {
    ### archive
    # highest.idx <- which.max(qualified$cor) # which.max always returns one index despite multiple equal maxs
    # if (highest.idx == 1) {
    #   dominant <- qualified[1, ]
    # } else {
    #   ratio <- qualified$cor[1:(highest.idx - 1)] / max(qualified$cor)
    #   if (all(ratio < 0.95)) { # heuristic threshold
    #     dominant <- qualified[highest.idx, ]
    #   } else { # similar in strength, choose the earliest
    #     dominant <- qualified[which(ratio >= 0.95)[1], ]
    #   }
    # }
    ### archive
    qualified.trend <- sign(diff(qualified$cor))
    print(qualified.trend)
    if (all(qualified.trend %in% c(-1, 0))) {
      dominant <- qualified[1, ]
    } else {
      # dominant <- qualified[1 + qualified.count, ] # a non-existent row, will output a row of NA
      dominant <- qualified[NA_integer_, ]
      message("non-monotonic")
    }

  }

  # check for trivial rhythms
  if (!is.na(dominant$lag)) {
    segment <- subset(ac, lag < dominant$lag) # segment preceding the dominant peak
    if (all(segment$cor >= 0)) { # in case no negative correlation occur before the dominant peak
      # dominant[1, ] <- c(NA, NA)
      dominant <- qualified[NA_integer_, ]
      message("trivial rhythms")
    }
  }

  # calculate heart rate
  dominant$hr <- 60 / (dominant$lag / sampling.rate)

  ### troubleshoot
  print(dominant)
  ###


  # results as a list
  list(ac.list = ac.list,
       # ac = ac,
       locmax = locmax,
       qualified = qualified,
       dominant = dominant,
       hr = dominant$hr)
}
