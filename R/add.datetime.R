#' Adding Local Datetime
#'
#' A helper function to convert UTC datetime to local datetime.
#'
#' @param df a data frame.
#' @param timezone a character string, local time zone.
#' @param reverse a logical, whether to convert local datetime to UTC datetime.
#' @param type an integer from 1 to 3. Use 1 to add datetime, 3 to add datetime, date, and time, and 2 to add date and time.
#'
#' @returns a data frame with additional columns.
#' @export
#'
#' @examples
#' # example a
#' df <- data.frame(datetime.UTC = as.POSIXct(c("2025-01-01 01:00:00",
#' "2025-07-01 01:00:00"), tz = "UTC"))
#' df
#' df1 <- add.datetime(df, type = 1, timezone = "")
#' df1
#' df1$datetime.UTC
#' df1$datetime
#'
#' df3 <- add.datetime(df, type = 3, timezone = "")
#' df3
#'
#'
#' # example b
#' data <- data.frame(datetime = as.POSIXct(c("2025-02-01 01:00:00", "2025-08-01 01:00:00")))
#' data$datetime
#' data2 <- add.datetime(data, type = 2)
#' data2
add.datetime <- function(df, type = 3, timezone, reverse = FALSE) {

  if (!reverse){ # normal case

    if (type %in% c(1, 3) && "datetime.UTC" %in% names(df)) {
      df <- df %>%
        mutate(
          datetime = as.POSIXct(.data$datetime.UTC, tz = timezone),
          .after = .data$datetime.UTC
        )
      # notice about time zone
      message("converting UTC to ", ifelse(timezone == "", Sys.timezone(), timezone), " time")
    }

    if (type %in% c(2, 3) && "datetime" %in% names(df)) {
      df <- df %>%
        mutate(
          date = as_date(.data$datetime),
          time = as_hms(.data$datetime),
          .after = .data$datetime
        )
    }
  } else { # reverse = TRUE, user wants to convert local time to UTC
    df <- df %>%
      mutate(
        datetime.UTC = as.POSIXct(.data$datetime, tz = "UTC"),
        .before = .data$datetime
      )
    message("adding UTC")
  }

  df
}
