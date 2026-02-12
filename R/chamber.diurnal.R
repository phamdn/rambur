#' Chamber Helper: Designing Diurnal Profile
#'
#' A helper function to design the diurnal patterns of environmental variables.
#'
#' @param day an integer, the day of experiment.
#' @param time.step a numeric, the resolution of the profile in hours.
#' @param light.duration a numeric, light duration (photoperiod) in hours, with light intensity of at least 1%.
#' @param peak.light.time a numeric, time of day when light intensity peaks.
#' @param mean.air.temp a numeric, the mean of air temperature during the day in °C.
#' @param range.air.temp a numeric, the range of air temperature during the day in °C.
#' @param mean.water.temp a numeric, the mean of water temperature during the day in °C.
#' @param range.water.temp a numeric, the range of water temperature during the day in °C.
#' @param peak.temp.time a numeric, time of day when temperature peaks.
#' @param tidal.cycle a vector of 0 or 1, the tidal cycle (e.g., semi-diurnal)
#' @param tidal.start.time a numeric, time of day when tidal cycle starts.
#' @param water.change.time a numeric, time of day when automatic water change starts.
#'
#' @returns a data frame with eight columns, including time as \code{day} and \code{hour}, and environmental variables
#' as \code{light}, \code{air.temp}, \code{water.temp}, \code{tide}, \code{exp.temp}, \code{wc}
#' @export
#'
#' @examples
#' # default
#' day0 <- chamber.diurnal()
#' day0
#'
#' # custom
#' chamber.diurnal(day = -3, time.step = 0.5,
#'   light.duration = 17, peak.light.time = 12,
#'   mean.air.temp = 30, range.air.temp = 10,
#'   mean.water.temp = 20, range.water.temp = 2, peak.temp.time = 14,
#'   tidal.cycle =  c(0, 1), tidal.start.time = 2.5,
#'   water.change.time = 16.5)
#'
chamber.diurnal <- function(day = 0, time.step = 1,
                       light.pattern = "normal.100", light.max = 100,
                       light.duration = 16, peak.light.time = 13,
                       mean.air.temp = 17, range.air.temp = 8,
                       mean.water.temp = 19, range.water.temp = 1,
                       peak.temp.time = 15,
                       tidal.cycle =  c(0, 1, 0, 1),
                       lunar.day = 24, tidal.start.time = 0,
                       water.change.time = NA
){

  # set time
  solar.day <- 24
  solar.steps <- solar.day / time.step

  if (solar.steps != round(solar.steps)) {
    stop("24 (h) divided by 'time.step' must result in a natural number.")
  }

  hour <- seq(from = 0, by = time.step, length.out = solar.steps)

  # day.dec <- day + hour / 24 # calculate day decimal

  # light
  if (light.pattern == "normal.100"){ # to be deprecated in future versions
  ## note the fact: dnorm(-3) / dnorm(0) * 100 =  1.1109 % need floor(), not round()
  simulated.light <- dnorm(hour,
                           mean = peak.light.time - time.step / 2, # continuity correction
                           sd = (light.duration - time.step) / 6) # three-sigma rule of thumb
  light <- simulated.light / max(simulated.light) * 100 # unit %
  light <- floor(light) # 0.6% will be 0%, not 1%
  }

  if (light.pattern == "normal"){
    simulated.light <- dnorm(hour,
                             mean = peak.light.time - time.step / 2,
                             sd = (light.duration - time.step) / 6)

    strong.light <- simulated.light / max(simulated.light) * 100
    strong.light <- floor(strong.light)

    reduced.light <- strong.light * (light.max / 100)
    reduced.light <- floor(reduced.light)

    light <- ifelse(strong.light >= 1 & reduced.light == 0, 1, reduced.light)
  }


  # air and water temperature
  simulated.temperature <-
    (sinpi((hour + 6 - peak.temp.time) / 12) + 1) / 2 # see plot(0:360, sinpi(0:360 / 180))

  air.temp <- (mean.air.temp - range.air.temp/2) + simulated.temperature * range.air.temp

  water.temp <- (mean.water.temp - range.water.temp/2) + simulated.temperature * range.water.temp

  # tide
  lunar.steps <- lunar.day / time.step
  steps.per.entry <- lunar.steps / length(tidal.cycle)

  if (steps.per.entry != round(steps.per.entry)) {
    stop("The length of 'tidal.cycle' vector should be 1, 2, or 4 (non-tidal, diurnal, or semidiurnal).")
  }

  tide <- rep(tidal.cycle, each = steps.per.entry) # has the length of lunar.steps NOT solar.steps

  # if (tidal.start.time > 0) { # not needed anymore
    steps.shift <- round(tidal.start.time / time.step) #add round to fix floating-point precision

    # if (steps.shift != round(steps.shift)) {
    #   stop("'tidal.start.time' divided by 'time.step' must result in a natural number.")
    # } dont use, cause error due to floating-point precision, consider all.equal in future

    tide <- c(
      tail(tide, steps.shift), # take some tail values and put forward
      head(tide, solar.steps - steps.shift) # take the head values and move behind
      # head(tide, - steps.shift) works in case of 24h lunar day but looks confusing
      # not work for tidal.start.time = 0 or lunar day > 24h
    ) # NOW has the length of solar.steps
  # }

  # exposure temperature
  exp.temp <- ifelse(tide == 0, air.temp, water.temp)

  # water change
  wc <- rep(0, solar.steps) # no water change as default

  if (!is.na(water.change.time)) wc[hour == water.change.time] <- 1

  # output
  output <- data.frame(day = day,
                       hour = hour,
                       # day.dec = day.dec,
                       light = light,
                       air.temp = round(air.temp, digits = 1),
                       water.temp = round(water.temp, digits = 1),
                       tide = tide,
                       exp.temp = round(exp.temp, digits = 1),
                       wc = wc
  )

  output
}
