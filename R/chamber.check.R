#' Chamber: Checking Performance
#'
#' @param design
#' @param actual
#' @param robo
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
chamber.check <- function(design, actual,
                          dttm.limits = c(NA, NA), dttm.breaks = waiver(), dttm.labels = waiver()){

  fig1 <- ggplot(data = design, aes(x = datetime, y = light)) +
    geom_step(linetype = 2) +
    geom_step(data = actual, color = 7) +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    labs(title = "Light", x = NULL, y = "%") +
    theme_minimal_grid()

  fig2 <- ggplot(data = design, aes(x = datetime, y = tide)) +
    geom_step(linetype = 2) +
    geom_step(data = actual, aes(y = actual.tide), color = 4) +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Tide", x = NULL, y = NULL) +
    theme_minimal_grid()

  temp.breaks <- pretty(range(design$exp.temp, actual$actual.temp, actual$room.temp))
  temp.breaks.range <- range(temp.breaks)

  fig3 <- ggplot(data = design, aes(x = datetime, y = exp.temp)) +
    geom_line(data = actual, aes(y = room.temp), color = 3) +
    geom_line(linetype = 2) +
    geom_line(data = actual, aes(y = actual.temp), color = 2) +
    scale_x_datetime(limits = as.POSIXct(dttm.limits),
                     date_breaks = dttm.breaks, date_labels = dttm.labels) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Exposure temperature", x = NULL, y = "°C", subtitle = "Room temperature") +
    theme_minimal_grid() +
    theme(
      plot.subtitle = element_text(color = 3, hjust = 0)
    )

  output <- plot_grid(fig1, fig2, fig3,
                             ncol = 1, rel_heights = c(1, 1, 2),
                             align = "hv")
  output

}
