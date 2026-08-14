#' Pulse: Extracting Heart Rate
#'
#' A function to extract heart rate from records of pulse devices.
#'
#' @param pulse.data a data frame, records of signal. Use output of \code{\link{pulse.read}}.
#' @param agg.res a character string, duration to summarize the mean or median of the records.
#' @param time.window a character string, window duration extract heart rate.
#' @param sampling.rate an integer, sampling rate in Hz. Can be autocalculated from data.
#' @param display a logical, whether to display plots.
#' @param score.parameter a numeric, the exponent used in the score.
#' @param cor.min a numeric, the correlation threshold for qualified signal.
#' @param search.scope a numeric, the ratio of lag domain length to the total signal length.
#'
#' @returns a list of two data frames, \code{window.hr} and \code{aggregated.hr} for window and summarized heart rate.
#' @export
#'
#' @seealso [pulse.hr()]
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' pulse.extract(pulse.data)
#' pulse.extract(pulse.data, agg.res = "15 minutes")
pulse.extract <- function(pulse.data,
                          sampling.rate = NULL,
                          search.scope = 1,
                          score.parameter = 0.5,
                          cor.min = 0.5,
                          display = "none",
                          time.window = "1 minute",
                          agg.res = NULL
                          ){

  # infer sampling rate Hz based on input data
  # use median, as the diff bw 2 timestamps are sometimes higher than usual, e.g., 00.390 - 59.989 = 0.41 s, not 0.2 s as typical for 5 Hz
  if (is.null(sampling.rate)) {
    sampling.rate <- round(1/median(as.numeric(diff(pulse.data$datetime))))
    message("assuming a sampling rate of ", sampling.rate, " Hz")
  }

  # using non-overlapping (sequential) windows, not overlapping (sliding) windows
  window.hr <- pulse.data %>%
    mutate(datetime = floor_date(.data$datetime, time.window)) %>%
    group_by(.data$datetime) %>%
    summarize(across(where(is.numeric), function(x) {
      if (display != "none") message(paste("datetime:", cur_group()$datetime, "| channel:", cur_column()))

      pulse.hr(x,
               sampling.rate = sampling.rate,
               search.scope = search.scope,
               score.parameter = score.parameter,
               cor.min = cor.min,
               display = display
      )$hr
    }
    )) %>%
    add.datetime(type = 2)

  output <- list(window.hr = window.hr)

  if (!is.null(agg.res)) {
    aggregated.hr <- window.hr %>%
      mutate(datetime = floor_date(.data$datetime, agg.res)) %>%
      group_by(.data$datetime) %>%
      summarize(across(where(is.numeric), mean)) |> # do not ignore NA heart rate \(x) mean(x, na.rm = FALSE)
      add.datetime(type = 2)

    output$aggregated.hr <- aggregated.hr

  }

  output
}
