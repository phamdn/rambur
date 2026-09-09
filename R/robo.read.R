#' Robomussel: Reading a Log File
#'
#' A function to read the CSV log file of a single robomussel or temperature EnvLogger.
#'
#' @param file.path a character string, path to the file.
#' @param metadata.lines an integer, number of lines to skip in the header.
#' @param timezone a character string, time zone.
#' @param agg.res a character string, duration to summarize the mean of the records. Optional.
#'
#' @returns a list of two data frames, \code{enhanced.data} and \code{aggregated.data} for enhanced and summarized records, respectively.
#' @export
#'
#' @examples
#' robo.file <- system.file("extdata/robo/RM1.csv", package = "rambur")
#' robo.read(robo.file, agg.res = "1 hour")
robo.read <- function(file.path,
                      metadata.lines = 21,
                      timezone = "",
                      agg.res = NULL){

  # notice about time zone
  message("reminder: robo log files were in UTC")

  # notice about time zone
  if (timezone == "")
    message("assuming ", Sys.timezone(), " as local time zone")
  else
    message("using ", timezone, " as local time zone")

  # retain metadata
  metadata <- paste(read_lines(file = file.path, n_max = metadata.lines),
                    collapse = "\n")

  original.data <- read_csv(file = file.path, skip = metadata.lines,
                            show_col_types = FALSE)
  # read_csv uses UTC as default (see col_datetime() and locale()),
  # which is the correct tz of robomussel (always UTC+0000)

  enhanced.data <- original.data |>
    transmute(datetime.UTC = .data$time,
              temp = .data$temp) |>
    add.datetime(type = 3, timezone = timezone)

  output <- list(
    metadata = metadata,
    # original.data = original.data,
    enhanced.data = enhanced.data
  )

  if (!is.null(agg.res)) {
    aggregated.data <- enhanced.data %>%
      mutate(datetime.UTC = floor_date(.data$datetime.UTC, agg.res)) %>%
      group_by(.data$datetime.UTC) %>%
      summarize(temp = mean(.data$temp)) %>%
      add.datetime(type = 3, timezone = timezone)

    output$aggregated.data <- aggregated.data

  }

  output

}
