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
#' rs <- pulse.read(folder)
#' rs
pulse.read <- function(folder.path = NULL,
                       file.name = "0000.CSV", size = c(1e6, 2e6),
                       metadata.lines = 22, timezone = ""){

  if (is.null(folder.path)) {
    folder.path <- getwd()
  }

  # get a list of all CSV files
  pulse.files <- list.files(path = folder.path, pattern = file.name, full.names = TRUE)

  # size limit
  pulse.files <- subset(pulse.files,
                        file.size(pulse.files) > size[1] & file.size(pulse.files) < size[2])

  # read and merge to a single original dataframe
  original.data <- read_csv(pulse.files, skip = metadata.lines,
                            col_names = c("time", paste0("channel", 1:10)),
                            # col_types = cols(time = col_datetime())
                            show_col_types = FALSE
                            )
  # read_csv uses UTC as default (see col_datetime() and locale()),
  # which is the correct tz of Pulse device (always UTC+0000)

  enhanced.data <- original.data %>%
    mutate(
      datetime = as.POSIXct(time, tz = timezone),
      date = as.Date(datetime, tz = timezone),
      time = as_hms(datetime), # as.Date is base R but as_hms is not
      .keep = "unused", .before = 1
    )

  list(original.data = original.data,
       enhanced.data = enhanced.data
  )
}
