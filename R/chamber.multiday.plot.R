#' Intertidal Chamber: Multiday Profile Visualization
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
#' heatwave.setup <- data.frame(day = seq(0, 34),
#' mean.air.temp =  c(rep(17, 7), seq(17, 29, 2), rep(31, 7), seq(29, 17, -2), rep(17, 7) ),
#' mean.water.temp = c(rep(19, 7), seq(19, 25, 1), rep(26, 7), seq(25, 19, -1), rep(19, 7) ) )
#'
#' rs <- chamber.multiday(heatwave.setup, start.date = "2025-05-30")
#'
#' chamber.multiday.plot(rs$output)
#'
chamber.multiday.plot <- function(df){

  temp.breaks <- pretty(range(df$exp.temp))
  temp.breaks.range <- range(temp.breaks)
  upper.x.axis <- function(x){
    df$day.dec[match(x, df$datetime)]
  }

  fig1 <- ggplot(df, aes(x = datetime, y = light)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "day")) +
    labs(title = "Light", x = "date", y = "%") +
    theme_minimal_grid()

  fig2 <- ggplot(df, aes(x = datetime, y = air.temp)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "day")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Air temperature", x = "date", y = "°C") +
    theme_minimal_grid()

  fig3 <- ggplot(df, aes(x = datetime, y = water.temp)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "day")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Water temperature", x = "date", y = "°C") +
    theme_minimal_grid()

  fig4 <- ggplot(df, aes(x = datetime, y = tide)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "day")) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Tide", x = "date", y = NULL) +
    theme_minimal_grid()

  fig5 <- ggplot(df, aes(x = datetime, y = exp.temp)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "day")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Exposure temperature", x = "date", y = "°C") +
    theme_minimal_grid()

  fig6 <- ggplot(df, aes(x = datetime, y = wc)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "day")) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Water change", x = "date", y = NULL) +
    theme_minimal_grid()

  output <- list(light = fig1,
                 air.temp = fig2,
                 water.temp = fig3,
                 tide = fig4,
                 exp.temp = fig5,
                 wc = fig6)

  output
}
