#' Robomussel: Reading of Single CSV Record
#'
#' @param file.path
#' @param metadata.lines
#' @param timezone
#'
#' @returns
#' @export
#'
#' @examples
#' file <- system.file("extdata/robomussel/RM1-04FD 6E00 220E 03-20250616 152857.csv", package = "rambur")
#' robomussel.read(file)
#'
robomussel.read <- function(file.path = NULL, metadata.lines = 21, timezone = ""){

  original.data <- read_csv(file = file.path, skip = metadata.lines,
                            show_col_types = FALSE)
  # read_csv uses UTC as default (see col_datetime() and locale()),
  # which is the correct tz of robomussel (always UTC+0000)

  enhanced.data <- original.data %>%
    transmute(
              # datetime = format(time, tz = timezone), not working, just <chr> format
              datetime = as.POSIXct(time, tz = timezone),
              date = as.Date(datetime), # as.Date is base R but as_hms is not
              time = as_hms(datetime),
              body.temp = temp
              )

  list(original.data = original.data,
       enhanced.data = enhanced.data
  )

}
