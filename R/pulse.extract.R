#' Pulse: Extracting Heart Rate
#'
#' @param data
#' @param summary.period
#' @param time.window
#' @param sampling.rate
#' @param cor.threshold
#' @param summary.fun
#'
#' @returns
#' @export
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' pulse.extract(pulse.data)
#' pulse.extract(pulse.data, summary.period = "30 minutes")
#' pulse.extract(pulse.data, summary.period = "30 minutes", summary.fun = "median")
pulse.extract <- function(data, sampling.rate = NULL,
                          score.exponents = c(2, 1), cor.threshold = 0.4,
                          diagnostics = FALSE,
                          time.window = "minute",
                          summary.period = NULL, summary.fun = "median",
                          nonNA.threshold = 0.1){

  # infer sampling rate Hz based on input data
  # use median, as the diff bw 2 timestamps are sometimes higher than usual, e.g., 00.390 - 59.989 = 0.41 s, not 0.2 s as typical for 5 Hz
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
                                                            score.exponents = score.exponents,
                                                            cor.threshold = cor.threshold,
                                                            diagnostics = diagnostics
                                                            )$hr
                     )) %>%
    mutate(date = as_date(datetime),
           time = as_hms(datetime),
           .after = datetime
    )

  output <- list(window.hr = window.hr)

  if (!is.null(summary.period)) {
    # helper for central tendency
    central <- function(x,
                        na.rm,
                        type = c("median", "mean")) {
      type <- match.arg(type)
      switch(type,
             mean = mean(x, na.rm = na.rm),
             median = median(x, na.rm = na.rm)
      )
    }

    summarized.hr <- window.hr %>%
      mutate(datetime = floor_date(datetime, summary.period)) %>%
      group_by(datetime) %>%
      # summarize(across(where(is.numeric), median, na.rm = TRUE)) %>% # use median, not mean, to alleviate the errors in heart rate calculation
      summarize(across(where(is.numeric),
                       ~ ifelse(mean(!is.na(.x)) >= nonNA.threshold, central(.x, na.rm = TRUE, type = summary.fun), NA)
      )) %>% # or only calculate with enough observations like more than 1/10 non missing
      mutate(date = as_date(datetime),
             time = as_hms(datetime),
             .after = datetime
      )

    output$summarized.hr <- summarized.hr

  }

  output
}
