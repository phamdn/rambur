#' Robomussel: Reading a CSV Record
#'
#' @param file.path
#' @param metadata.lines
#' @param time.zone
#'
#' @returns
#' @export
#'
#' @examples
#' file <- system.file("extdata/robo/RM1-04FD 6E00 220E 03-20250616 152857.csv", package = "rambur")
#' rs <- robo.read(file, time.zone = "Europe/Berlin")
#' rs
robo.read <- function(file.path = NULL,
                      metadata.lines = 21, time.zone = "", summary.period = "hour"){

  original.data <- read_csv(file = file.path, skip = metadata.lines,
                            show_col_types = FALSE)
  # read_csv uses UTC as default (see col_datetime() and locale()),
  # which is the correct tz of robomussel (always UTC+0000)

  enhanced.data <- original.data %>%
    transmute(datetime.UTC = time,
              # datetime = format(time, tz = time.zone), not working, just <chr> format
              datetime = as.POSIXct(time, tz = time.zone),
              date = as.Date(datetime, tz = time.zone),
              time = as_hms(datetime), # as.Date is base R but as_hms is not
              body.temp = temp
           )

  summarized.data <- enhanced.data %>%
    mutate(datetime = floor_date(datetime, summary.period)) %>%
    group_by(datetime) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

  list(
       enhanced.data = enhanced.data,
       summarized.data = summarized.data
  )

}
