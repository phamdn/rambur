#' Chamber: Designing Diurnal Profile
#'
#' A worker function to design the diurnal patterns of environmental variables.
#'
#' @param day an integer, the day of experiment.
#' @param time.step a numeric, the resolution of the profile in hours.
#' @param light.duration a numeric, light duration (photoperiod) in hours, with light intensity of at least 1%.
#' @param light.peak.time a numeric, time of day when light intensity peaks.
#' @param temp.air.mean a numeric, the mean of air temperature during the day in °C.
#' @param temp.air.range a numeric, the range of air temperature during the day in °C.
#' @param temp.water.mean a numeric, the mean of water temperature during the day in °C.
#' @param temp.water.range a numeric, the range of water temperature during the day in °C.
#' @param temp.peak.time a numeric, time of day when temperature peaks.
#' @param ie.cycle a vector of 0 and 1, the tidal cycle (e.g., semi-diurnal)
#' @param iec.start.time a numeric, time of day when tidal cycle starts.
#' @param wc.time a numeric, time of day when automatic water change starts.
#' @param light.model a character string, pattern of light. Default to "gaussian", indicating gaussian function.
#' @param light.max an integer, maximum light intensity.
#' @param tidal.day a numeric, tidal day duration in hours. Default to 24, same as the solar day.
#' @param temp.model a character string, pattern of temperature. Default to "sinusoidal", indicating sinusoidal function.
#'
#' @returns a data frame with eight columns, including time as \code{day} and \code{hour}, and environmental variables
#' as \code{light}, \code{temp.air}, \code{temp.water}, \code{immersion}, \code{temp}, \code{wc}.
#' @export
#'
#' @examples
#' # default
#' day0 <- chamber.diurnal()
#' day0
#'
#' # custom
#' chamber.diurnal(day = -3, time.step = 0.5,
#'   light.duration = 17, light.peak.time = 12,
#'   temp.air.mean = 30, temp.air.range = 10,
#'   temp.water.mean = 20, temp.water.range = 2, temp.peak.time = 14,
#'   ie.cycle =  c(0, 1), iec.start.time = 2.5,
#'   wc.time = 16.5)
#'
chamber.diurnal <- function(day = 0, time.step = 1,

                       light.model = "gaussian",
                       light.duration = 16,
                       light.peak.time = 13,
                       light.max = 100,

                       temp.model = "sinusoidal",
                       temp.air.mean = 17,
                       temp.air.range = 8,
                       temp.water.mean = 19,
                       temp.water.range = 1,
                       temp.peak.time = 15,

                       tidal.day = 24,
                       ie.cycle =  c(0, 1, 0, 1),
                       iec.start.time = 0,

                       wc.time = NA
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

  if (light.model == "gaussian.100"){ # to be deprecated in future versions
  ## note the fact: dnorm(-3) / dnorm(0) * 100 =  1.1109 % need floor(), not round()
  simulated.light <- dnorm(hour,
                           mean = light.peak.time - time.step / 2, # continuity correction
                           sd = (light.duration - time.step) / 6) # three-sigma rule of thumb
  light <- simulated.light / max(simulated.light) * 100 # unit %
  light <- floor(light) # 0.6% will be 0%, not 1%
  }

  if (light.model == "gaussian"){

    if (light.duration <= time.step || light.max == 0) {
      light <- rep(0, solar.steps)
    } else if (light.duration > solar.day) {
      light <- rep(light.max, solar.steps)
    } else {
      simulated.light <- dnorm(hour,
                               mean = light.peak.time - time.step / 2,
                               sd = (light.duration - time.step) / 6)

      strong.light <- simulated.light / max(simulated.light) * 100
      strong.light <- floor(strong.light)

      reduced.light <- strong.light * (light.max / 100)
      reduced.light <- floor(reduced.light)

      light <- ifelse(strong.light >= 1 & reduced.light == 0, 1, reduced.light)
    }

  }

  else if (light.model == "uniform"){
    sunrise <- light.peak.time - light.duration / 2
    sunset <- light.peak.time + light.duration / 2
    light <- ifelse(hour >= sunrise & hour < sunset, floor(light.max), 0)
  }
  #
  # else if (light.model == "random"){
  #   set.seed(seed)
  #   light <- sample(1 : light.max, size = solar.steps, replace = TRUE)
  # }

  # air and water temperature
  if (temp.model == "sinusoidal") {
    simulated.temp <-
      (sinpi((hour + 6 - temp.peak.time) / 12) + 1) / 2 # see plot(0:360, sinpi(0:360 / 180))

    temp.air <- (temp.air.mean - temp.air.range/2) + simulated.temp * temp.air.range

    temp.water <- (temp.water.mean - temp.water.range/2) + simulated.temp * temp.water.range
  }

  # immersion
  tidal.steps <- tidal.day / time.step
  steps.per.entry <- tidal.steps / length(ie.cycle)

  if (steps.per.entry != round(steps.per.entry)) {
    stop("The length of 'ie.cycle' vector should be 1, 2, or 4 (non-tidal, diurnal, or semidiurnal).")
  }

  immersion <- rep(ie.cycle, each = steps.per.entry) # has the length of tidal.steps NOT solar.steps

  # if (iec.start.time > 0) { # not needed anymore
    steps.shift <- round(iec.start.time / time.step) #add round to fix floating-point precision

    # if (steps.shift != round(steps.shift)) {
    #   stop("'iec.start.time' divided by 'time.step' must result in a natural number.")
    # } dont use, cause error due to floating-point precision, consider all.equal in future

    immersion <- c(
      tail(immersion, steps.shift), # take some tail values and put forward
      head(immersion, solar.steps - steps.shift) # take the head values and move behind
      # head(immersion, - steps.shift) works in case of 24h tidal day but looks confusing
      # not work for iec.start.time = 0 or tidal day > 24h
    ) # NOW has the length of solar.steps
  # }

  # exposure temperature
  temp <- ifelse(immersion == 0, temp.air, temp.water)

  # water change
  wc <- rep(0, solar.steps) # no water change as default

  if (!is.na(wc.time)) wc[hour == wc.time] <- 1

  # output
  output <- data.frame(day = day,
                       hour = hour,
                       # day.dec = day.dec,
                       light = light,
                       temp.air = round(temp.air, digits = 1),
                       temp.water = round(temp.water, digits = 1),
                       immersion = immersion,
                       temp = round(temp, digits = 1),
                       wc = wc
  )

  class(output) <- c("chamber.diurnal", "data.frame")
  output
}
