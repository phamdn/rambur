#' Pulse: Checking Photoplethysmogram (PPG) Signal
#'
#' @param data
#' @param channel
#' @param dttm.limits
#'
#' @returns
#' @export
#'
#' @examples
pulse.check <- function(data, channel, dttm.limits = c(NA, NA)){

  fig1 <- ggplot(data, aes(x = datetime, y = .data[[channel]])) +
    geom_line() +
    scale_x_datetime(limits = as.POSIXct(dttm.limits)) +
    theme_minimal_grid()

  print(fig1)

  subset(data, datetime >= dttm.limits[1] & datetime <= dttm.limits[2], channel)

}
