#' Adding Local Datetime
#'
#' A helper function to convert UTC datetime to local datetime, then break it into date and time.
#'
#' @param df a data frame.
#' @param timezone a character string, local time zone.
#' @param reverse a logical, whether to add datetime.UTC column.
#'
#' @returns
#' @export
#'
#' @examples
add.datetime <- function(df, timezone, reverse = FALSE) {

  if (!reverse){
    if (!"datetime" %in% names(df)) { # if datetime missing
      df <- df %>%
        mutate(
          datetime = as.POSIXct(.data$datetime.UTC, tz = timezone),
          .after = .data$datetime.UTC
        )
      # notice about time zone
      message("converting UTC to ", ifelse(timezone == "", Sys.timezone(), timezone), " time")
    }

    if (!"date" %in% names(df) || !"time" %in% names(df)) { # if date or time missing
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
