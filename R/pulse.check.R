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
pulse.check <- function(data, channel,
                        dttm.limits = c(NA, NA),
                        sampling.rate = NULL, cor.threshold = 0.4
                        ){

  # infer sampling rate Hz based on input data
  if (is.null(sampling.rate)) {
    sampling.rate <- round(1/median(as.numeric(diff(data$datetime))))
    message("using sampling rate of ", sampling.rate, " Hz")
  }

  fig1 <- ggplot(data, aes(x = datetime, y = .data[[channel]])) +
    geom_line() +
    scale_x_datetime(limits = as.POSIXct(dttm.limits)) +
    theme_minimal_grid()

  print(fig1)

  signal <- subset(data, datetime >= dttm.limits[1] & datetime <= dttm.limits[2], channel)

  output <- pulse.hr(signal = signal,
                     sampling.rate = sampling.rate,
                     cor.threshold = cor.threshold)

  plot(output$ac.list)

  output

}
