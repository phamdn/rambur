#' Intertidal Chamber: Reading Multiple CSV Records
#'
#' @param timezone
#' @param folder.path
#' @param metadata.lines
#'
#' @returns
#' @export
#'
#' @examples
#' folder <- system.file("extdata/chamber", package = "rambur")
#' chamber.data <- chamber.read(folder, timezone = "Europe/Berlin")
#' chamber.data
chamber.read <- function(folder.path = NULL, file.name = ".CSV",
                         metadata.lines = 16, timezone = "", summary.period = "hour"){

  if (is.null(folder.path)) {
    folder.path <- getwd()
  }

  # get a list of all CSV files
  chamber.files <- list.files(path = folder.path, pattern = file.name, full.names = TRUE)

  # read and merge to a single original dataframe
  original.data <- read_csv(chamber.files, skip = metadata.lines,
                             col_types = cols(`LED_intensity_%` = col_double())
                            # otherwise, LED was character, e.g., "000"
  ) # Note that Chamber records local time, not UTC like Pulse or Robo

  # presence of "Reset" lines in CSV, i.e., when a chamber was reset
  # problems <- problems(original.data)

  # make some new columns and retain all unused columns
  enhanced.data <-
    na.omit(original.data) %>% # remove Reset lines otherwise as.POSIXct() returns error
    mutate(
      datetime = as.POSIXct(paste(Date, Time), tz = timezone),
      date = Date, # just <date> character/format from original data in local time zone
      time = Time,

      design.temp = Top_setpoint,
      actual.temp1 = T1,
      actual.temp2 = T2,
      actual.temp3 = T3,
      actual.temp = (T1 + T2 + T3)/3, # should be the same as Top_avg unless rounding issue
                                      # simply use mean() or sd() will not perform row-wise calculation
      # actual.temp.sd = apply(across(T1:T3), 1, sd),
      # actual.temp.diff = apply(across(T1:T3), 1, function(x) diff(range(x))),
      room.temp = T6,

      design.tide = Tide,
      working.tide = Tide_pump_state,
      actual.tide1 = WS1,
      actual.tide2 = WS2,
      actual.tide3 = WS3,
      actual.tide = (WS1 + WS2 + WS3)/3,

      min.water1 = WS6,
      min.water2 = WS7,
      min.water3 = WS8,
      min.water = (WS6 + WS7 + WS8)/3,
      wc.out = Outlet_valve_state,
      wc.in = Inlet_valve_state,

      light = `LED_intensity_%`,

      .keep = "unused", # can change to "none" to save space
      .before = 1
    )

  summarized.data <- enhanced.data %>%
    mutate(datetime = floor_date(datetime, summary.period)) %>%
    group_by(datetime) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    mutate(date = as_date(datetime), # better than as.Date(datetime, tz = timezone)
           time = as_hms(datetime),
           .after = datetime
           )

  list(original.data = original.data,
       # problems = problems,
       enhanced.data = enhanced.data,
       summarized.data = summarized.data
       )
}
