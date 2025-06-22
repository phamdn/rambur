#' Intertidal chamber multiday profile visualization
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

  fig <- ggplot(df, aes(x = datetime, y = exp.temp)) +
    geom_line() +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Exposure temperature", x = NULL, y = "°C") +
    scale_x_datetime(sec.axis = dup_axis(labels = function(x){
      df$day.dec[match(x, df$datetime)]
      })) +
    theme_minimal_grid()

  fig
}
