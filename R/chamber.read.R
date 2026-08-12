#' Chamber: Reading Log Files
#'
#' A function to read the CSV log files of an intertidal chamber.
#'
#' @param timezone a character string, time zone.
#' @param folder.path a character string, path to the folder of CSV records.
#' @param metadata.lines an integer, number of lines to skip in the header.
#' @param file.name a character string, filter the file name.
#' @param size.limits a vector of two numerics, minimum and maximum size in bytes.
#' @param agg.res a character string, duration to summarize the mean of the records.
#'
#' @returns a list of three data frames, \code{original.data}, \code{enhanced.data}, and \code{aggregated.data} for original, enhanced, and summarized records, respectively.
#' @export
#'
#' @examples
#' folder <- system.file("extdata/chamber", package = "rambur")
#' chamber.data <- chamber.read(folder)
#' chamber.data
chamber.read <- function(folder.path = NULL,
                         file.name = ".CSV",
                         size.limits = c(0, Inf),
                         metadata.lines = 16,
                         timezone = "",
                         agg.res = "1 minute"){

  # notice about time zone
  message("reminder: chamber log files were in local time")

  if (is.null(folder.path)) {
    folder.path <- getwd()
    message("reading from the current working directory")
  }

  # get a list of all CSV files
  chamber.files <- list.files(path = folder.path, pattern = file.name, full.names = TRUE)

  # size limit
  chamber.files <- subset(chamber.files,
                        file.size(chamber.files) > size.limits[1] &
                          file.size(chamber.files) < size.limits[2])

  message("importing ", length(chamber.files), " files")

  # notice about time zone
  message("assuming ", ifelse(timezone == "", Sys.timezone(), timezone), " as local time zone")

  # read and merge to a single original dataframe
  original.data <- read_csv(chamber.files, skip = metadata.lines,
                             col_types = cols(`LED_intensity_%` = col_double()) # otherwise, LED was character, e.g., "000"
  )

  # presence of "Reset" lines in CSV, i.e., when a chamber was reset
  # problems <- problems(original.data)

  # make some new columns and retain all unused columns
  enhanced.data <-
    na.omit(original.data) %>% # remove Reset lines otherwise as.POSIXct() returns error
    mutate(
      datetime = as.POSIXct(paste(.data$Date, .data$Time), tz = timezone),
      date = .data$Date, # just <date> character/format from original data in local time zone
      time = .data$Time,

      target.temp = .data$Top_setpoint,
      actual.temp1 = .data$T1,
      actual.temp2 = .data$T2,
      actual.temp3 = .data$T3,
      actual.temp = (.data$T1 + .data$T2 + .data$T3)/3, # should be the same as Top_avg unless rounding issue
                                      # simply use mean() or sd() will not perform row-wise calculation
      # actual.temp.sd = apply(across(T1:T3), 1, sd),
      # actual.temp.diff = apply(across(T1:T3), 1, function(x) diff(range(x))),
      ambient.temp = .data$T6,

      stor.target.temp = .data$Base_setpoint,
      stor.actual.temp1 = .data$T4,
      stor.actual.temp2 = .data$T5,
      stor.actual.temp = (.data$T4 + .data$T5)/2,

      target.immersion = .data$Tide,
      tide.pump = .data$Tide_pump_state,
      actual.immersion1 = .data$WS1,
      actual.immersion2 = .data$WS2,
      actual.immersion3 = .data$WS3,
      actual.immersion = (.data$WS1 + .data$WS2 + .data$WS3)/3,

      stor.min.water1 = .data$WS6,
      stor.min.water2 = .data$WS7,
      stor.min.water3 = .data$WS8,
      stor.min.water = (.data$WS6 + .data$WS7 + .data$WS8)/3,
      stor.max.water.em = .data$WS4,
      stor.max.water.im = .data$WS5,

      wc.out = .data$Outlet_valve_state,
      wc.in = .data$Inlet_valve_state,

      actual.light = .data$`LED_intensity_%`,

      heat.lamps = .data$`Heat_Lamps_%`,
      circulation.fans = .data$Circle_fan_state,
      exhaust.fans = .data$Cool_fan_state,
      heating.rod = .data$Water_Heater_state,
      water.chiller = .data$Water_Cooler_state,
      chiller.pump = .data$Cooler_pump_state,

      .keep = "unused", # can change to "none" to save disk space
      .before = 1
    )

  aggregated.data <- enhanced.data %>%
    mutate(datetime = floor_date(.data$datetime, agg.res)) %>%
    group_by(.data$datetime) %>%
    # summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>% # will also summarize cols such as designed.temp, which is meaningless
    # better to be more selective in what to summarize here
    summarize(
      across(c(.data$actual.light, .data$tide.pump, .data$actual.immersion, .data$actual.temp),
                     mean) # use na.rm will require \(x) mean(x, ...) or function(x) mean(x, ...), see ?across
      ) %>%
    add.datetime(type = 2)

  list(original.data = original.data, # keep to understand NA problems
       enhanced.data = enhanced.data,
       aggregated.data = aggregated.data
       )
}
