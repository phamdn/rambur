#' Robomussels: Reading Multiple CSV Records
#'
#' A function to read the CSV Records of multiple robomussels.
#'
#' @param folder.path a character string, path to the folder.
#' @param file.name a character string, filter the file name.
#' @param metadata.lines an integer, number of lines to skip in the header.
#' @param timezone a character string, time zone.
#' @param summary.period a character string, duration to summarize the mean of the records. Optional.
#'
#' @returns a list of three data frames, \code{enhanced.data}, \code{synchronized.data}, and \code{summarized.data} for enhanced, synchronized, and summarized records, respectively.
#' @export
#'
#' @examples
#' robo.folder <- system.file("extdata/robo", package = "rambur")
#' robo.read2(robo.folder)
#' robo.read2(robo.folder, summary.period = "hour")
robo.read2 <- function(folder.path = NULL,
                       file.name = ".csv",
                      metadata.lines = 21,
                      timezone = "",
                      summary.period = NULL){

  if (is.null(folder.path)) {
    folder.path <- getwd()
    message("reading from the current working directory")
  }

  # get a list of all CSV files
  robo.files <- list.files(path = folder.path, pattern = file.name, full.names = TRUE)

  message("importing ", length(robo.files), " files")

  # notice about time zone
  if (timezone == "") {
    message("using ", Sys.timezone(), " time zone")
  }

  original.data <- lapply(robo.files, function(x){
     read_csv(file = x, skip = metadata.lines,
                              show_col_types = FALSE)
    # read_csv uses UTC as default (see col_datetime() and locale()),
    # which is the correct tz of robomussel (always UTC+0000)

  })

  enhanced.data <- lapply(original.data, function(x){
    x %>%
      transmute(datetime.UTC = .data$time,
                # datetime = format(time, tz = timezone), not working, just <chr> format
                datetime = as.POSIXct(.data$time, tz = timezone),
                date = as_date(.data$datetime),
                time = as_hms(.data$datetime), # as.Date is base R but as_date and as_hms is not
                body.temp = .data$temp
      ) # transmute() is better than mutate() for keeping columns in desired order, note the repurposed use of "time"
  })

  # resolve the clock drift issue

  # synchronized.data <- lapply(enhanced.data, function(x){
  #   x %>%
  #     transmute(datetime = floor_date(datetime, "minute"),
  #               body.temp
  #               )
  # })
  #
  # synchronized.data <- lapply(seq_along(synchronized.data), function(i){
  #   df <- synchronized.data[[i]]
  #   names(df)[2] <- paste0("body.temp", i)
  #   df
  # })

  synchronized.data <- imap(enhanced.data, \(x, idx) {
    x %>%
      transmute(
        datetime = floor_date(.data$datetime, "minute"),
        !!paste0("body.temp", idx) := .data$body.temp
      )
  }) %>%
    reduce(full_join, by = "datetime") %>%
    mutate(date = as_date(.data$datetime),
           time = as_hms(.data$datetime),
           .after = .data$datetime
    ) %>%
    mutate(
      body.temp = rowMeans(across(starts_with("body.temp")), na.rm = TRUE)
    )

  output <- list(
    # original.data = original.data,
    enhanced.data = enhanced.data,
    synchronized.data = synchronized.data
  )

  if (!is.null(summary.period)) {
    summarized.data <- synchronized.data %>%
      mutate(datetime = floor_date(.data$datetime, summary.period)) %>%
      group_by(.data$datetime) %>%
      summarize(body.temp = mean(.data$body.temp, na.rm = TRUE)) %>%
      mutate(date = as_date(.data$datetime),
             time = as_hms(.data$datetime),
             .after = .data$datetime
      )


    output$summarized.data <- summarized.data
  }

  output
}
