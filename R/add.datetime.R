#' Adding Local Datetime
#'
#' A helper function to convert UTC to local datetime, then add date and time columns.
#'
#' @param df
#' @param timezone
#'
#' @returns
#' @export
#'
#' @examples
add.datetime <- function(df, timezone) {

  if ("datetime.UTC" %in% names(df)) {
    df <- df %>%
      mutate(
        datetime = as.POSIXct(.data$datetime.UTC, tz = timezone),
        date = as_date(.data$datetime),
        time = as_hms(.data$datetime),
        .after = .data$datetime.UTC
      )

    # notice about time zone
    # need to check main functions to avoid duplicate messages
    if (timezone == "") {
      message("converting UTC to ", Sys.timezone(), " time")
    } else {
      message("converting UTC to ", timezone, " time")
    }
  }

  else if ("datetime" %in% names(df)) {
    df <- df %>%
      # mutate(
      #   datetime.UTC = as.POSIXct(.data$datetime, tz = "UTC"),
      #   .before = .data$datetime
      # ) |>
      mutate(
        date = as_date(.data$datetime),
        time = as_hms(.data$datetime),
        .after = .data$datetime
      )
  }

  df
}
