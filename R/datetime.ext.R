#' Expand datetime
#'
#' @param df
#' @param timezone
#'
#' @returns
#' @export
#'
#' @examples
datetime.ext <- function(df, timezone = "") {

  if ("datetime.UTC" %in% names(df)) {
    df <- df %>%
      mutate(
        datetime = as.POSIXct(.data$datetime.UTC, tz = timezone),
        date = as_date(.data$datetime),
        time = as_hms(.data$datetime),
        .after = .data$datetime.UTC
      )
  }

  else if ("datetime" %in% names(df)) {
    df <- df %>%
      mutate(
        date = as_date(.data$datetime),
        time = as_hms(.data$datetime),
        .after = .data$datetime
      )
  }

  df
}
