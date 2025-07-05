#' Intertidal Chamber: Diurnal Profile Visualization
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
#' rs <- chamber.diurnal()
#'
#' chamber.diurnal.plot(rs)
#'
chamber.diurnal.plot <- function(df){

  temp.breaks <- pretty(range(df$air.temp, df$water.temp))
  temp.breaks.range <- range(temp.breaks)
  hour.breaks <- seq(0, 24, 6)

  fig1 <- ggplot(df, aes(x = hour, y = light)) +
    geom_point() +
    geom_step() +
    geom_line(linetype = 2) +
    scale_x_continuous(breaks = hour.breaks) +
    labs(title = "Light", y = "%") +
    theme_cowplot()

  fig2 <- ggplot(df, aes(x = hour, y = air.temp)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Air temperature", y = "°C") +
    theme_cowplot()

  fig3 <- ggplot(df, aes(x = hour, y = water.temp)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Water temperature", y = "°C") +
    theme_cowplot()

  fig4 <- ggplot(df, aes(x = hour, y = tide)) +
    geom_point() +
    geom_step() +
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Tide", y = NULL) +
    theme_cowplot()

  fig5 <- ggplot(df, aes(x = hour, y = exp.temp)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Exposure temperature", y = "°C") +
    theme_cowplot()

  fig6 <- ggplot(df, aes(x = hour, y = wc)) +
    geom_point() +
    geom_step() +
    scale_x_continuous(breaks = hour.breaks) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Water change", y = NULL) +
    theme_cowplot()

  output <- plot_grid(fig1, fig2, fig3, fig4, fig5, fig6,
                      align = "hv")

  output
}

