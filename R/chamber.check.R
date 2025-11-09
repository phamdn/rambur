#' Chamber: Checking Compliance
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
#' acc.setup <- data.frame(day = seq(0, 34),
#' mean.air.temp = 17, mean.water.temp = 19)
#'
#' acc.profile <- chamber.design(acc.setup,
#'                               start.date = "2025-05-15", export = FALSE)
#'
#' folder <- system.file("extdata/chamber", package = "rambur")
#' chamber.data <- chamber.read(folder)
#'
#' chamber.check(acc.profile$expansion, chamber.data$enhanced.data)
#'
chamber.check <- function(design, actual,
                          time.range = c(NA, NA)){

  fig1 <- ggplot(data = design, aes(x = datetime, y = light)) +
    geom_step(linetype = 2) +
    geom_step(data = actual, color = 7) +
    scale_x_datetime(limits = as.POSIXct(time.range)) +
    labs(title = "Light", x = NULL, y = "%") +
    theme_minimal_grid()

  fig2 <- ggplot(data = design, aes(x = datetime, y = tide)) +
    geom_step(linetype = 2) +
    geom_step(data = actual, aes(y = actual.tide), color = 4) +
    scale_x_datetime(limits = as.POSIXct(time.range)) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Tide", x = NULL, y = NULL) +
    theme_minimal_grid()

  temp.breaks <- pretty(range(design$exp.temp, actual$actual.temp, actual$room.temp))
  temp.breaks.range <- range(temp.breaks)

  fig3 <- ggplot(data = design, aes(x = datetime, y = exp.temp)) +
    geom_line(data = actual, aes(y = room.temp), color = 3) +
    geom_line(linetype = 2) +
    geom_line(data = actual, aes(y = actual.temp), color = 2) +
    scale_x_datetime(limits = as.POSIXct(time.range)) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Temperature", x = NULL, y = "°C") +
    theme_minimal_grid()

  output <- plot_grid(fig1, fig2, fig3,
                             ncol = 1, rel_heights = c(1, 1, 2),
                             align = "hv")
  output

}
