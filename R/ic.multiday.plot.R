#' Intertidal chamber multiday profile visualization
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
#' control.setup <- data.frame(day = seq(0, 30),
#'   mean.air.temp = 19,
#'   mean.water.temp = 19)
#'
#' rs <- ic.multiday(control.setup)
#'
#' ic.multiday.plot(rs)
ic.multiday.plot <- function(df){
  fig <- ggplot(df, aes(x = datetime, y = exp.temp)) +
    geom_line() +
    scale_y_continuous(n.breaks = 10) +
    labs(title = "Exposure temperature", x = NULL, y = "°C") +
    theme_minimal_grid()

  fig
}
