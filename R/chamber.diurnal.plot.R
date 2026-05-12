#' Chamber Helper: Plotting Diurnal Profile
#'
#' A helper function to plot the diurnal patterns of environmental variables.
#'
#' @param df a data frame, output of \code{\link{chamber.diurnal}}.
#'
#' @returns a six-panel plot of environmental variables.
#' @export
#'
#' @examples
#' # default
#' day0 <- chamber.diurnal()
#' chamber.diurnal.plot(day0)
#'
chamber.diurnal.plot <- function(df){

  temp.breaks <- pretty(range(df$temp.air, df$temp.water))
  temp.breaks.range <- range(temp.breaks)
  hour.breaks <- seq(0, 24, 6)

  fig1 <- ggplot(df, aes(x = .data$hour, y = .data$light)) +
    geom_step(color = 1, linetype = 2) +
    geom_point(color = 7) + # color = 7
    # geom_line(linetype = 2) + # normal curve
    scale_x_continuous(breaks = hour.breaks) +
    labs(title = "Light", y = "%", x = "") +
    theme_minimal_grid()

  fig2 <- ggplot(df, aes(x = .data$hour, y = .data$temp.air)) +
    geom_line(color = 1, linetype = 2) +
    geom_point(color = 2, shape = 17) + # color = 2
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Air temperature", y = "\u00B0C", x = "") +
    theme_minimal_grid()

  fig3 <- ggplot(df, aes(x = .data$hour, y = .data$temp.water)) +
    geom_line(color = 1, linetype = 2) +
    geom_point(color = 2, shape = 15) + #color = 2
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Water temperature", y = "\u00B0C", x = "") +
    theme_minimal_grid()

  fig4 <- ggplot(df, aes(x = .data$hour, y = .data$tide)) +
    geom_step(color = 1, linetype = 2) +
    geom_point(color = 4) + #color = 4
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Tide", y = NULL, x = "") +
    theme_minimal_grid()

  fig5 <- ggplot(df, aes(x = .data$hour, y = .data$temp)) +
    geom_line(color = 1, linetype = 2) +
    geom_point(color = 2, aes(shape = as.factor(.data$tide))) + #color = 2
    scale_shape_manual(values = c(17, 15)) +
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Exposure temperature", y = "\u00B0C", x = "Time of day (h)") +
    theme_minimal_grid() +
    theme(legend.position = "none")

  fig6 <- ggplot(df, aes(x = .data$hour, y = .data$wc)) +
    geom_step(color = 1, linetype = 2) +
    geom_point(color = 1) + #color = 8
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Water change", y = NULL, x = "") +
    theme_minimal_grid()

  output <- plot_grid(fig1, fig2, fig3, fig4, fig5, fig6,
                      align = "hv")

  output
}

