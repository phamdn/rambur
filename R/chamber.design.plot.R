#' Chamber: Plotting Multiday Profile
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
#' # acclimation phase
#' acc.setup <- data.frame(day = seq(0, 34),
#' mean.air.temp = 17, mean.water.temp = 19)
#'
#' acc.profile <- chamber.design(acc.setup,
#' start.date = "2025-05-15", export = FALSE)
#'
#' chamber.design.plot(acc.profile$expansion)
#'
chamber.design.plot <- function(df){

  temp.breaks <- pretty(range(df$exp.temp))
  temp.breaks.range <- range(temp.breaks)
  upper.x.axis <- function(x){ # x is datetime (the tick marks of the primary x-axis)
    df$day[match(x, df$datetime)] # find the corresponding day of that datetime
  }

  fig1 <- ggplot(df, aes(x = datetime, y = light)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    labs(title = "Light", x = NULL, y = "%") +
    theme_minimal_grid()

  fig2 <- ggplot(df, aes(x = datetime, y = air.temp)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Air temperature", x = NULL, y = "°C") +
    theme_minimal_grid()

  fig3 <- ggplot(df, aes(x = datetime, y = water.temp)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Water temperature", x = NULL, y = "°C") +
    theme_minimal_grid()

  fig4 <- ggplot(df, aes(x = datetime, y = tide)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Tide", x = NULL, y = NULL) +
    theme_minimal_grid()

  fig5 <- ggplot(df, aes(x = datetime, y = exp.temp)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Exposure temperature", x = NULL, y = "°C") +
    theme_minimal_grid()

  fig6 <- ggplot(df, aes(x = datetime, y = wc)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Water change", x = NULL, y = NULL) +
    theme_minimal_grid()

  output <- list(light = fig1,
                 air.temp = fig2,
                 water.temp = fig3,
                 tide = fig4,
                 exp.temp = fig5,
                 wc = fig6)

  output
}
