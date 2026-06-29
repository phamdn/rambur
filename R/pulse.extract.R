#' Pulse: Extracting Heart Rate
#'
#' A function to extract heart rate from records of pulse devices.
#'
#' @param data a data frame, records of signal. Use output of \code{\link{pulse.read}}.
#' @param summary.period a character string, duration to summarize the mean or median of the records.
#' @param time.window a character string, window duration extract heart rate.
#' @param sampling.rate an integer, sampling rate in Hz. Can be autocalculated from data.
#' @param summary.fun a character string, central tendancy to summarize.
#' @param display a logical, whether to plot display.
#' @param nonNA.threshold a numeric, only summarize when the proportion of non missing values exceeds this threshold.
#' @param score.parameter
#' @param cor.min
#'
#' @returns a list of two data frames, \code{window.hr} and \code{summarized.hr} for window and summarized heart rate.
#' @export
#'
#' @seealso [pulse.hr()]
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' pulse.extract(pulse.data)
#' pulse.extract(pulse.data, summary.period = "15 minutes")
#' pulse.extract(pulse.data, summary.period = "15 minutes", summary.fun = "median")
pulse.extract <- function(data, sampling.rate = NULL,
                          score.method = "exponential",
                          score.parameter = 0.1, cor.min = 0.5,
                          display = "none",
                          time.window = "minute",
                          summary.period = NULL, summary.fun = "mean",
                          nonNA.threshold = 0){

  # infer sampling rate Hz based on input data
  # use median, as the diff bw 2 timestamps are sometimes higher than usual, e.g., 00.390 - 59.989 = 0.41 s, not 0.2 s as typical for 5 Hz
  if (is.null(sampling.rate)) {
    sampling.rate <- round(1/median(as.numeric(diff(data$datetime))))
    message("using sampling rate of ", sampling.rate, " Hz")
  }

  # using non-overlapping (sequential) windows, not overlapping (sliding) windows
  window.hr <- data %>%
    mutate(datetime = floor_date(.data$datetime, time.window)) %>%
    group_by(.data$datetime) %>%
    summarize(across(where(is.numeric), function(x) {
      if (display %in% c("all", "ppg")) message(paste("datetime:", cur_group()$datetime, "| channel:", cur_column()))

      pulse.hr(x,
               sampling.rate = sampling.rate,
               score.method = score.method,
               score.parameter = score.parameter,
               cor.min = cor.min,
               display = display
      )$hr
    }
    )) %>%
    mutate(date = as_date(.data$datetime),
           time = as_hms(.data$datetime),
           .after = .data$datetime
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
      mutate(datetime = floor_date(.data$datetime, summary.period)) %>%
      group_by(.data$datetime) %>%
      # summarize(across(where(is.numeric), median, na.rm = TRUE)) %>% # use median, not mean, to alleviate the errors in heart rate calculation
      summarize(across(where(is.numeric),
                       ~ ifelse(mean(!is.na(.x)) >= nonNA.threshold,
                                central(.x, na.rm = TRUE, type = summary.fun),
                                NA)
      )) %>% # or only calculate with enough observations e.g. more than 1/10 non missing
      mutate(date = as_date(.data$datetime),
             time = as_hms(.data$datetime),
             .after = .data$datetime
      )

    output$summarized.hr <- summarized.hr

  }

  output
}
