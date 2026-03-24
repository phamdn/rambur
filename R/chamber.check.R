#' Chamber: Checking Performance
#'
#' A function to compare the design and the actual performance of the chamber.
#'
#' @param design a data frame, multiday patterns of environmental variables. Use \code{design} output of \code{\link{chamber.design}}.
#' @param chamber.data a data frame, actual records of environmental variables. Use \code{enhanced.data} output of \code{\link{chamber.read}}.
#' @param robo.data a data frame, actual records of robomussels. Optional.
#' @param dttm.limits a vector of two character strings, limits of dates.
#' @param dttm.breaks a character string, duration between date breaks.
#' @param dttm.labels a character string, format of dates.
#'
#' @returns a three-panel plot of light, tide, and temperature.
#' @export
#'
#' @examples
#'
#' acc.daily.settings <- data.frame(day = seq(0, 34),
#' mean.air.temp = 17, mean.water.temp = 19)
#'
#' acc <- chamber.design(acc.daily.settings,
#'                               start.date = "2025-05-15", export = FALSE)
#'
#' folder <- system.file("extdata/chamber", package = "rambur")
#' chamber.data <- chamber.read(folder)
#'
#' chamber.check(acc$design, chamber.data$enhanced.data, dttm.limits = c("2025-05-15", "2025-05-20"))
#'
chamber.check <- function(design, chamber.data, robo.data = NULL,
                          dttm.limits = c(NA, NA), dttm.breaks = waiver(), dttm.labels = waiver()){

  fig1 <- ggplot(data = design, aes(x = .data$datetime, y = .data$light, color = "Designed")) +
    geom_step(linetype = 2) +
    geom_step(data = chamber.data, aes(y = .data$actual.light, color = "Actual"), alpha = 0.8) +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    scale_color_manual(values = c("Designed" = 1, "Actual" = 7), breaks = c("Designed", "Actual")) +
    labs(title = "Light", x = NULL, y = "%", color = NULL) +
    theme_minimal_grid() +
    theme(legend.position = "top")

  fig2 <- ggplot(data = design, aes(x = .data$datetime, y = .data$tide, color = "Designed")) +
    geom_step(linetype = 2) +
    geom_step(data = chamber.data, aes(y = .data$tide.pump, color = "Pump"), alpha = 0.5) +
    geom_step(data = chamber.data, aes(y = .data$actual.tide, color = "Actual"), alpha = 0.8) +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    scale_color_manual(values = c("Designed" = 1, "Actual" = 4, "Pump" = 6), breaks = c("Designed", "Actual", "Pump")) +
    labs(title = "Tide", x = NULL, y = NULL, color = NULL) +
    theme_minimal_grid() +
    theme(legend.position = "top")

  temp.breaks <- pretty(range(design$exp.temp,
                              chamber.data$actual.temp,
                              chamber.data$room.temp,
                              robo.data$body.temp,
                              na.rm = TRUE))
  temp.breaks.range <- range(temp.breaks)

  fig3 <- ggplot(data = design, aes(x = .data$datetime, y = .data$exp.temp, color = "Designed")) +
    geom_line(data = chamber.data, aes(y = .data$room.temp, color = "Room"), alpha = 0.8) + # plot room temp first as background
    geom_line(linetype = 2) +
    geom_line(data = chamber.data, aes(y = .data$actual.temp, color = "Actual"), alpha = 0.8) +
    {
      if (!is.null(robo.data))
        geom_line(data = robo.data, aes(y = .data$body.temp, color = "Body"), alpha = 0.8) # plot body temp last
    } +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    scale_color_manual(values = c("Designed" = 1, "Actual" = 2,
                                  "Body" = 8, "Room" = 3),
                       breaks = c("Designed", "Actual", "Body", "Room")) +
    labs(title = "Temperature", x = NULL, y = "\u00B0C", color = NULL) + # title = "Exposure temperature"
    theme_minimal_grid() +
    theme(legend.position = "top")

  output <- plot_grid(fig1, fig2, fig3,
                             ncol = 1, rel_heights = c(1, 1, 2),
                             align = "hv")
  output

}
