#' Robomussel: Reading a Log File
#'
#' A function to read the CSV log file of a single robomussel or temperature EnvLogger.
#'
#' @param file.path a character string, path to the file.
#' @param metadata.lines an integer, number of lines to skip in the header.
#' @param timezone a character string, time zone.
#' @param summary.period a character string, duration to summarize the mean of the records. Optional.
#'
#' @returns a list of two data frames, \code{enhanced.data} and \code{summarized.data} for enhanced and summarized records, respectively.
#' @export
#'
#' @examples
#' robo.file <- system.file("extdata/robo/RM1.csv", package = "rambur")
#' robo.read(robo.file)
#' robo.read(robo.file, summary.period = "hour")
robo.read <- function(file.path,
                      metadata.lines = 21,
                      timezone = "",
                      summary.period = NULL){

  # notice about time zone
  if (timezone == "") {
    message("using ", Sys.timezone(), " time zone")
  }

  # metadata
  metadata <- paste(read_lines(file = file.path, n_max = metadata.lines),
                    collapse = "\n")

  original.data <- read_csv(file = file.path, skip = metadata.lines,
                            show_col_types = FALSE)
  # read_csv uses UTC as default (see col_datetime() and locale()),
  # which is the correct tz of robomussel (always UTC+0000)

  enhanced.data <- original.data %>%
    transmute(datetime.UTC = .data$time,
              # datetime = format(time, tz = timezone), not working, just <chr> format
              datetime = as.POSIXct(.data$datetime.UTC, tz = timezone),
              date = as_date(.data$datetime),
              time = as_hms(.data$datetime), # as.Date is base R but as_date and as_hms is not
              temp = .data$temp
           ) # transmute() is better than mutate() for keeping columns in desired order, note the repurposed use of "time"

  output <- list(
    metadata = metadata,
    # original.data = original.data,
    enhanced.data = enhanced.data
  )

  if (!is.null(summary.period)) {
    summarized.data <- enhanced.data %>%
      mutate(datetime = floor_date(.data$datetime, summary.period)) %>%
      group_by(.data$datetime) %>%
      summarize(n = sum(!is.na(.data$temp)),
                temp = mean(.data$temp)) %>%
      mutate(date = as_date(.data$datetime),
             time = as_hms(.data$datetime),
             .after = .data$datetime
      )

    output$summarized.data <- summarized.data

  }

  output

}
