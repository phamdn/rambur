#' Intertidal chamber diurnal profile generation
#'
#' @param day
#' @param time.step
#' @param tidal.cycle
#' @param tc.start.time
#' @param mean.air.temp
#' @param range.air.temp
#' @param mean.water.temp
#' @param range.water.temp
#' @param peak.temp.time
#' @param light.duration
#' @param peak.light.time
#' @param water.change.time
#'
#' @returns
#' @export
#'
#' @examples
#' # default
#' ic.diurnal()
#'
#' # custom
#' ic.diurnal(time.step = 0.5,
#'   tc.start.time = 2.5,
#'   water.change.time = 15.5)
ic.diurnal <- function(day = 0, time.step = 1,
                       light.duration = 16, peak.light.time = 13,
                       mean.air.temp = 17, range.air.temp = 8,
                       mean.water.temp = 19, range.water.temp = 1,
                       peak.temp.time = 15,
                       tidal.cycle =  c(0, 1, 0, 1), tc.start.time = 0,
                       water.change.time = NA
){

  # set time
  steps <- 24 / time.step

  if (steps != round(steps)) {
    stop("24 (h) divided by 'time.step' must result in a whole number.")
  }

  hour <- seq(from = 0, by = time.step, length.out = steps)

  day.dec <- day + hour / 24 # calculate day decimal

  # light
  simulated.light <- dnorm(hour,
                           mean = peak.light.time - time.step/2, # continuity correction
                           sd = (light.duration - time.step) / 6) # three-sigma rule of thumb

  light <- simulated.light / max(simulated.light) * 100 # unit %

  # air and water temperature
  simulated.temperature <-
    (sinpi((hour + 6 - peak.temp.time) / 12) + 1) / 2 # see plot(0:360, sinpi(0:360 / 180))

  air.temp <- (mean.air.temp - range.air.temp/2) + simulated.temperature * range.air.temp

  water.temp <- (mean.water.temp - range.water.temp/2) + simulated.temperature * range.water.temp

  # tide
  steps.per.entry <- steps / length(tidal.cycle)

  if (steps.per.entry != round(steps.per.entry)) {
    stop("The length of 'tidal.cycle' vector should be 1, 2, or 4 (non-tidal, diurnal, or semidiurnal).")
  }

  tide <- rep(tidal.cycle, each = steps.per.entry)

  if (tc.start.time > 0) {
    steps.shift <- tc.start.time / time.step
    tide <- c(
      tail(tide, steps.shift),
      head(tide, -steps.shift)
    )
  }

  # exposure temperature
  exp.temp <- ifelse(tide == 0, air.temp, water.temp)

  # water change
  wc <- rep(0, steps) # no water change as default

  if (!is.na(water.change.time)) wc[hour == water.change.time] <- 1

  # output
  output <- data.frame(day = day,
                       hour = hour,
                       day.dec = day.dec,
                       light = floor(light), # 0.6% will be 0%, not 1%
                       air.temp = round(air.temp, digits = 1),
                       water.temp = round(water.temp, digits = 1),
                       tide = tide,
                       exp.temp = round(exp.temp, digits = 1),
                       wc = wc
  )

  output
}
