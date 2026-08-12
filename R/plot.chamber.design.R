#' Chamber: Plotting Multiday Profile
#'
#' A helper function to plot the multiday patterns of environmental variables.
#'
#' @param x a data frame, multiday patterns of environmental variables. Use \code{design} output of \code{\link{chamber.design}}.
#' @param ... additional arguments, currently ignored.
#'
#' @returns a list of six plots of environmental variables.
#' @method plot chamber.design
#' @export
#'
#' @examples
#' # acclimation phase
#' acc.setup <- data.frame(day = seq(0, 34),
#' temp.air.mean = 17, temp.water.mean = 19)
#'
#' acc.profile <- chamber.design(acc.setup,
#' start.date = "2025-05-15", export = FALSE)
#'
#' plot(acc.profile$design)
#'
plot.chamber.design <- function(x, ...){

  df <- x

  temp.breaks <- pretty(range(df$temp))
  temp.breaks.range <- range(temp.breaks)
  upper.x.axis <- function(x){ # x is datetime (the tick marks of the primary x-axis)
    df$day[match(x, df$datetime)] # find the corresponding day of that datetime
  }

  fig1 <- ggplot(df, aes(x = .data$datetime, y = .data$light)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    labs(title = "Light", x = NULL, y = "%") +
    theme_minimal_grid()

  fig2 <- ggplot(df, aes(x = .data$datetime, y = .data$temp.air)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Air temperature", x = NULL, y = "\u00B0C") +
    theme_minimal_grid()

  fig3 <- ggplot(df, aes(x = .data$datetime, y = .data$temp.water)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Water temperature", x = NULL, y = "\u00B0C") +
    theme_minimal_grid()

  fig4 <- ggplot(df, aes(x = .data$datetime, y = .data$immersion)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Immersion", x = NULL, y = NULL) +
    theme_minimal_grid()

  fig5 <- ggplot(df, aes(x = .data$datetime, y = .data$temp)) +
    geom_line() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = temp.breaks, limits = temp.breaks.range) +
    labs(title = "Exposure temperature", x = NULL, y = "\u00B0C") +
    theme_minimal_grid()

  fig6 <- ggplot(df, aes(x = .data$datetime, y = .data$wc)) +
    geom_step() +
    scale_x_datetime(sec.axis = dup_axis(labels = upper.x.axis, name = "Day of exposure")) +
    scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) +
    labs(title = "Water change", x = NULL, y = NULL) +
    theme_minimal_grid()

  output <- list(light = fig1,
                 temp.air = fig2,
                 temp.water = fig3,
                 immersion = fig4,
                 temp = fig5,
                 wc = fig6)

  output
}
