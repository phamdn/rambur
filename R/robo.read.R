#' Robomussel: Reading a Single CSV Record
#'
#' @param file.path
#' @param metadata.lines
#' @param timezone
#' @param summary.period
#'
#' @returns
#' @export
#'
#' @examples
#' robo.file <- system.file("extdata/robo/RM1-04FD 6E00 220E 03-20250616 152857.csv", package = "rambur")
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

  original.data <- read_csv(file = file.path, skip = metadata.lines,
                            show_col_types = FALSE)
  # read_csv uses UTC as default (see col_datetime() and locale()),
  # which is the correct tz of robomussel (always UTC+0000)

  enhanced.data <- original.data %>%
    transmute(datetime.UTC = time,
              # datetime = format(time, tz = timezone), not working, just <chr> format
              datetime = as.POSIXct(time, tz = timezone),
              date = as_date(datetime),
              time = as_hms(datetime), # as.Date is base R but as_date and as_hms is not
              body.temp = temp
           ) # transmute() is better than mutate() for keeping columns in desired order, note the repurposed use of "time"

  output <-   list(
    # original.data = original.data,
    enhanced.data = enhanced.data
  )

  if (!is.null(summary.period)) {
    summarized.data <- enhanced.data %>%
      mutate(datetime = floor_date(datetime, summary.period)) %>%
      group_by(datetime) %>%
      summarize(body.temp = mean(body.temp, na.rm = TRUE)) %>%
      mutate(date = as_date(datetime),
             time = as_hms(datetime),
             .after = datetime
      )

    output$summarized.data <- summarized.data

  }

  output

}
