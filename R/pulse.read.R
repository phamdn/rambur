#' Pulse: Reading Multiple CSV Records
#'
#' @param timezone
#' @param folder.path
#' @param metadata.lines
#' @param file.name
#' @param size
#'
#' @returns
#' @export
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder, timezone = "Europe/Berlin")
#' pulse.data
pulse.read <- function(folder.path = NULL,
                       file.name = "0000.CSV", size.limits = c(1000e3, 1500e3),
                       metadata.lines = 22,
                       timezone = ""){

  if (is.null(folder.path)) {
    folder.path <- getwd()
    message("reading from the current working directory")
  }

  # get a list of all CSV files
  pulse.files <- list.files(path = folder.path, pattern = file.name, full.names = TRUE)

  # size limit
  pulse.files <- subset(pulse.files,
                        file.size(pulse.files) > size.limits[1] &
                          file.size(pulse.files) < size.limits[2])

  message("importing ", length(pulse.files), " files")

  # notice about time zone
  if (timezone == "") {
    message("using ", Sys.timezone(), " time zone")
  }

  # read and merge to a single original dataframe
  original.data <- read_csv(pulse.files, skip = metadata.lines,
                            col_names = c("time", paste0("channel.", 1:10)), # need to improve to retain original sample names
                            # col_types = cols(time = col_datetime())
                            show_col_types = FALSE
                            )
  # read_csv uses UTC as default (see col_datetime() and locale()),
  # which is the correct tz of Pulse device (always UTC+0000)

  enhanced.data <- original.data %>%
    mutate(
      datetime.UTC = time,
      datetime = as.POSIXct(time, tz = timezone),
      date = as_date(datetime),
      time = as_hms(datetime), # as.POSIXct and as.Date are base R but as_date and as_hms not
      .keep = "unused", .before = 1
    )

  # take too much space to return both
  # list(original.data = original.data,
  #      enhanced.data = enhanced.data
  # )

  enhanced.data
}
