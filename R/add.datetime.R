#' Adding Local Datetime
#'
#' A helper function to convert UTC datetime to local datetime, then break it into date and time.
#'
#' @param df a data frame.
#' @param timezone a character string, local time zone.
#' @param reverse a logical, whether to convert local datetime to UTC datetime.
#'
#' @returns a data frame with additional columns.
#' @export
#'
#' @examples
#' # example a
#' df <- data.frame(datetime.UTC = as.POSIXct(c("2025-01-01 01:00:00", "2025-07-01 01:00:00"), tz = "UTC"))
#' df2 <- add.datetime(df, timezone = "")
#' df2
#' df2$datetime.UTC
#' df2$datetime
#'
#' # example b
#' df3 <- data.frame(datetime = as.POSIXct(c("2025-02-01 01:00:00", "2025-08-01 01:00:00")))
#' df3$datetime
#' df4 <- add.datetime(df3)
#' df4
add.datetime <- function(df, timezone, reverse = FALSE) {

  if (!reverse){
    if ("datetime" %notin% names(df)) { # if datetime missing
      df <- df %>%
        mutate(
          datetime = as.POSIXct(.data$datetime.UTC, tz = timezone),
          .after = .data$datetime.UTC
        )
      # notice about time zone
      message("converting UTC to ", ifelse(timezone == "", Sys.timezone(), timezone), " time")
    }

    if ("date" %notin% names(df) || "time" %notin% names(df)) { # if date or time missing
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
