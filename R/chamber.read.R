#' Intertidal Chamber: Read CSV Output
#'
#' @param folder
#' @param skip.metadata
#' @param timezone
#'
#' @returns
#' @export
#'
#' @examples
#' folder <- system.file("extdata/chamber", package = "rambur")
#' chamber.read(folder)
chamber.read <- function(folder = NULL, metadata.lines = 16, timezone = ""){

  files.chamber <- list.files(path = folder, pattern = ".CSV", full.names = TRUE)

  original.data <- read_csv(files.chamber, skip = metadata.lines,
                             col_types = cols(`LED_intensity_%` = col_double())
  )


  selected.data <- na.omit(original.data) %>% transmute(date = Date,
                                         time = Time,
                                         datetime = as.POSIXct(paste(Date, Time)),
                                         design.temp = Top_setpoint,
                                         actual.temp1 = T1,
                                         actual.temp2 = T2,
                                         actual.temp3 = T3,
                                         actual.temp = Top_avg,
                                         room.temp = T6,
                                         design.tide = Tide,
                                         actual.tide1 = WS1,
                                         actual.tide2 = WS2,
                                         actual.tide3 = WS3,
                                         actual.tide = as.double(WS1 | WS2 | WS3),
                                         light = `LED_intensity_%`,
                                         outlet = Outlet_valve_state,
                                         inlet = Inlet_valve_state
  )

  list(original.data = original.data,
       selected.data = selected.data
       )
}
