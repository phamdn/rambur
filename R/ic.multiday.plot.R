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
#' rs <- ic.multiday(heatwave.setup)
#'
#' ic.multiday.plot(rs)
ic.multiday.plot <- function(df){

  temp.breaks <- pretty(range(df$exp.temp))

  fig <- ggplot(df, aes(x = datetime, y = exp.temp)) +
    geom_line() +
    scale_y_continuous(breaks = temp.breaks, limits = range(temp.breaks)) +
    labs(title = "Exposure temperature", x = NULL, y = "°C") +
    theme_minimal_grid()

  fig
}
