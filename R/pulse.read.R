#' Pulse: Reading Log Files
#'
#' A function to read the CSV log files of a PULSE V2 logger.
#'
#' @param timezone a character string, time zone.
#' @param folder.path a character string, path to the folder of CSV records.
#' @param metadata.lines an integer, number of lines to skip in the header.
#' @param size.limits a vector of two numerics, minimum and maximum file sizes in bytes, e.g., 1e6 bytes (~1 MB).
#' @param file.name a character string, filter the file name.
#'
#' @returns a data frame of enhanced data.
#' @export
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' pulse.data
pulse.read <- function(folder.path = NULL,
                       file.name = ".CSV",
                       size.limits = c(0, Inf),
                       metadata.lines = 22,
                       channel.names = NULL,
                       timezone = ""){

  # notice about time zone
  message("reminder: pulse log files were in UTC")

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

  if (is.null(channel.names)) {
    channel.names <- paste0("channel.", 1:10)
  } else stopifnot("10 channel names are required" = length(channel.names) == 10)

  # read and merge to a single original dataframe
  original.data <- read_csv(pulse.files, skip = metadata.lines,
                            col_names = c("time", channel.names),
                            # can be improve to extract original sample names in log files
                            # col_types = cols(time = col_datetime())
                            show_col_types = FALSE
                            )
  # read_csv uses UTC as default (see col_datetime() and locale()), which is the correct tz of Pulse device (always UTC+0000)

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
