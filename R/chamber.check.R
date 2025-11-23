#' Chamber: Checking Performance
#'
#' @param design
#' @param actual
#' @param robo
#' @param dttm.limits
#' @param dttm.breaks
#' @param dttm.labels
#'
#' @returns
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
chamber.check <- function(design, actual, robo = NULL,
                          dttm.limits = c(NA, NA), dttm.breaks = waiver(), dttm.labels = waiver()){

  fig1 <- ggplot(data = design, aes(x = datetime, y = light, color = "Designed")) +
    geom_step(linetype = 2) +
    geom_step(data = actual, aes(y = working.light, color = "Working"), alpha = 0.8) +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    scale_color_manual(values = c("Designed" = 1, "Working" = 7), breaks = c("Designed", "Working")) +
    labs(title = "Light", x = NULL, y = "%", color = NULL) +
    theme_minimal_grid() +
    theme(legend.position = "top")

  fig2 <- ggplot(data = design, aes(x = datetime, y = tide, color = "Designed")) +
    geom_step(linetype = 2) +
    geom_step(data = actual, aes(y = actual.tide, color = "Actual"), alpha = 0.8) +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    scale_color_manual(values = c("Designed" = 1, "Actual" = 4), breaks = c("Designed", "Actual")) +
    labs(title = "Tide", x = NULL, y = NULL, color = NULL) +
    theme_minimal_grid() +
    theme(legend.position = "top")

  temp.breaks <- pretty(range(design$exp.temp, actual$actual.temp, actual$room.temp, robo$body.temp))
  temp.breaks.range <- range(temp.breaks)

  fig3 <- ggplot(data = design, aes(x = datetime, y = exp.temp, color = "Designed")) +
    geom_line(data = actual, aes(y = room.temp, color = "Room temperature"), alpha = 0.8) + # plot room temp first as background
    geom_line(linetype = 2) +
    geom_line(data = actual, aes(y = actual.temp, color = "Actual"), alpha = 0.8) +
    {
      if (!is.null(robo))
        geom_line(data = robo, aes(y = body.temp, color = "Body"), alpha = 0.8) # plot body temp last
    } +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    scale_color_manual(values = c("Designed" = 1, "Actual" = 2,
                                  "Body" = 8, "Room temperature" = 3),
                       breaks = c("Designed", "Actual", "Body", "Room temperature")) +
    labs(title = "Temperature", x = NULL, y = "°C", color = NULL) + # title = "Exposure temperature"
    theme_minimal_grid() +
    theme(legend.position = "top")

  output <- plot_grid(fig1, fig2, fig3,
                             ncol = 1, rel_heights = c(1, 1, 2),
                             align = "hv")
  output

}
