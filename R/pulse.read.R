#' Pulse: Reading Multiple CSV Records
#'
#' A function to read the CSV records of pulse devices.
#'
#' @param timezone a character string, time zone.
#' @param folder.path a character string, path to the folder of CSV records.
#' @param metadata.lines an integer, number of lines to skip in the header.
#' @param size.limits a vector of two numerics, minimum and maximum file sizes in bytes.
#' @param file.name a character string, filter the file name.
#'
#' @returns a data frame of enhanced records.
#' @export
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' pulse.data
pulse.read <- function(folder.path = NULL,
                       file.name = "0000.CSV", size.limits = c(1000e3, 2000e3),
                       metadata.lines = 22,
                       timezone = ""){

  if (is.null(folder.path)) {
    folder.path <- getwd()
    message("reading from the current working directory")
  }

  # get a list of CSV files with matched names
  pulse.files <- list.files(path = folder.path, pattern = file.name, full.names = TRUE)

  # filter by size limits
  pulse.files <- subset(pulse.files,
                        file.size(pulse.files) >= size.limits[1] &
                          file.size(pulse.files) <= size.limits[2])

  message("importing ", length(pulse.files), " files")

  # notice about time zone
  # if (timezone == "") {
  #   message("using ", Sys.timezone(), " time zone")
  # }

  # read and merge to a single original dataframe
  original.data <- read_csv(pulse.files, skip = metadata.lines,
                            col_names = c("time", paste0("channel.", 1:10)), # need to improve to retain original sample names
                            # col_types = cols(time = col_datetime())
                            show_col_types = FALSE
                            )
  # read_csv uses UTC as default (see col_datetime() and locale()),
  # which is the correct tz of Pulse device (always UTC+0000)

  # enhanced.data <- original.data %>%
  #   mutate(
  #     datetime.UTC = .data$time,
  #     datetime = as.POSIXct(.data$time, tz = timezone),
  #     date = as_date(.data$datetime),
  #     time = as_hms(.data$datetime), # as.POSIXct and as.Date are base R but as_date and as_hms not
  #     .keep = "unused", .before = 1
  #   )

  enhanced.data <- original.data %>%
    mutate(
      datetime.UTC = .data$time,
      .keep = "unused", .before = 1
    ) %>%
    add.datetime(timezone = timezone)

  # take too much space to return both
  # list(original.data = original.data,
  #      enhanced.data = enhanced.data
  # )

  enhanced.data
}
