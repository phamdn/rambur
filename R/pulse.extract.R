#' Pulse: Extracting Heart Rate
#'
#' @param data
#' @param summary.period
#' @param time.window
#' @param sampling.rate
#' @param cor.threshold
#'
#' @returns
#' @export
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' pulse.extract(pulse.data, cor.threshold = 0.4, summary.period = "30 minutes")
pulse.extract <- function(data,
                          sampling.rate = NULL, cor.threshold = 0.7,
                          time.window = "minute", summary.period = "hour"){

  # infer sampling rate Hz based on input data
  if (is.null(sampling.rate)) {
    sampling.rate <- round(1/median(as.numeric(diff(data$datetime))))
    message("using sampling rate of ", sampling.rate, " Hz")
  }

  # using non-overlapping (sequential) windows, not overlapping (sliding) windows
  window.hr <- data %>%
    mutate(datetime = floor_date(datetime, time.window)) %>%
    group_by(datetime) %>%
    summarize(across(where(is.numeric), function(x) pulse.hr(x,
                                                            sampling.rate = sampling.rate,
                                                            cor.threshold = cor.threshold)$hr)) %>%
    mutate(date = as_date(datetime),
           time = as_hms(datetime),
           .after = datetime
    )

  summarized.hr <- window.hr %>%
    mutate(datetime = floor_date(datetime, summary.period)) %>%
    group_by(datetime) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    mutate(date = as_date(datetime),
           time = as_hms(datetime),
           .after = datetime
    )


  list(window.hr = window.hr,
       summarized.hr = summarized.hr)
}
