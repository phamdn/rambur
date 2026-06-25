#' Helper: Convert and Expand Datetime UTC
#'
#' @param df
#' @param timezone
#'
#' @returns
#' @export
#'
#' @examples
datetime <- function(df, timezone = "") {

  if ("datetime.UTC" %in% names(df)) {
    df <- df %>%
      mutate(
        datetime = as.POSIXct(.data$datetime.UTC, tz = timezone),
        date = as_date(.data$datetime),
        time = as_hms(.data$datetime),
        .after = .data$datetime.UTC
      )

    # notice about time zone
    if (timezone == "") {
      message("using ", Sys.timezone(), " time zone")
    } # need to check double message in main functions
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
