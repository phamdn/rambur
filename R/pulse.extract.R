#' Pulse: Extracting Heart Rate
#'
#' @param data
#' @param summary.period
#' @param time.window
#'
#' @returns
#' @export
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder, timezone = "Europe/Berlin")
#' pulse.extract(pulse.data, timezone = "Europe/Berlin")
pulse.extract <- function(data, time.window = "minute", summary.period = "hour", timezone = ""){

  # using non-overlapping (sequential) windows, not overlapping (sliding) windows
  window.hr <- data %>%
    mutate(datetime = floor_date(datetime, time.window)) %>%
    group_by(datetime) %>%
    summarize(across(where(is.numeric), function(x) hr.calc(x)$hr)) %>%
    mutate(date = as.Date(datetime, tz = timezone),
           time = as_hms(datetime),
           .after = datetime
    )

  summarized.hr <- window.hr %>%
    mutate(datetime = floor_date(datetime, summary.period)) %>%
    group_by(datetime) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    mutate(date = as.Date(datetime, tz = timezone),
           time = as_hms(datetime),
           .after = datetime
    )


  list(window.hr = window.hr,
       summarized.hr = summarized.hr)
}
