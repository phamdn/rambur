#' Intertidal chamber diurnal profile visualization
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
#' rs <- ic.diurnal()
#'
#' ic.diurnal.plot(rs)
ic.diurnal.plot <- function(df){

  temp.breaks <- pretty(range(df$air.temp, df$water.temp))

  fig1 <- ggplot(data = df, aes(x = hour, y = light)) +
    geom_point() +
    geom_step() +
    geom_line(linetype = 2) +
    scale_x_continuous(breaks = seq(0, 24, 6)) +
    labs(title = "Light", y = "%") +
    theme_cowplot()

  fig2 <- ggplot(data = df, aes(x = hour, y = air.temp)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 24, 6)) +
    scale_y_continuous(breaks = temp.breaks, limits = range(temp.breaks)) +
    labs(title = "Air temperature", y = "°C") +
    theme_cowplot()

  fig3 <- ggplot(data = df, aes(x = hour, y = water.temp)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 24, 6)) +
    scale_y_continuous(breaks = temp.breaks, limits = range(temp.breaks)) +
    labs(title = "Water temperature", y = "°C") +
    theme_cowplot()

  fig4 <- ggplot(data = df, aes(x = hour, y = tide)) +
    geom_point() +
    geom_step() +
    scale_x_continuous(breaks = seq(0, 24, 6)) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Tide", y = NULL) +
    theme_cowplot()

  fig5 <- ggplot(data = df, aes(x = hour, y = exp.temp)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 24, 6)) +
    scale_y_continuous(breaks = temp.breaks, limits = range(temp.breaks)) +
    labs(title = "Exposure temperature", y = "°C") +
    theme_cowplot()

  fig6 <- ggplot(data = df, aes(x = hour, y = wc)) +
    geom_point() +
    geom_step() +
    scale_x_continuous(breaks = seq(0, 24, 6)) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Water change", y = NULL) +
    theme_cowplot()

  output <- plot_grid(fig1, fig2, fig3, fig4, fig5, fig6,
                      align = "hv")

  output
}

