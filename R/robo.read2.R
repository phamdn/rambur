#' RobomusselS: Reading CSV Log Files
#'
#' A function to read the log files of multiple robomussels or temperature EnvLoggers.
#'
#' @param folder.path a character string, path to the folder.
#' @param file.name a character string, filter the file name.
#' @param metadata.lines an integer, number of lines to skip in the header.
#' @param timezone a character string, time zone.
#' @param agg.res a character string, duration to summarize the mean of the records. Optional.
#'
#' @returns a list of three data frames, \code{enhanced.data}, \code{synchronized.data}, and \code{aggregated.data} for enhanced, synchronized, and aggregated data, respectively.
#' @export
#'
#' @examples
#' robo.folder <- system.file("extdata/robo", package = "rambur")
#' robo.read2(robo.folder, agg.res = "hour")
robo.read2 <- function(folder.path = NULL,
                       file.name = ".csv",
                      metadata.lines = 21,
                      timezone = "",
                      agg.res = NULL){

  if (is.null(folder.path)) {
    folder.path <- getwd()
    message("reading from the current working directory")
  }

  # get a list of all CSV files
  robo.files <- list.files(path = folder.path, pattern = file.name, full.names = TRUE)

  message("importing ", length(robo.files), " files")

  # notice about time zone
  # if (timezone == "") {
  #   message("using ", Sys.timezone(), " time zone")
  # }

  original.data <- lapply(robo.files, function(x){
     read_csv(file = x, skip = metadata.lines,
                              show_col_types = FALSE)
    # read_csv uses UTC as default (see col_datetime() and locale()),
    # which is the correct tz of robomussel (always UTC+0000)

  })

  enhanced.data <- lapply(original.data, function(x){
    x %>%
      # transmute(datetime.UTC = .data$time,
      #           # datetime = format(time, tz = timezone), not working, just <chr> format
      #           datetime = as.POSIXct(.data$time, tz = timezone),
      #           date = as_date(.data$datetime),
      #           time = as_hms(.data$datetime), # as.Date is base R but as_date and as_hms is not
      #           temp = .data$temp
      # ) # transmute() is better than mutate() for keeping columns in desired order, note the repurposed use of "time"
      transmute(datetime.UTC = .data$time,
                temp = .data$temp) |>
      add.datetime(timezone = timezone)
  })

  # resolve the clock drift issue

  # synchronized.data <- lapply(enhanced.data, function(x){
  #   x %>%
  #     transmute(datetime = floor_date(datetime, "minute"),
  #               temp
  #               )
  # })
  #
  # synchronized.data <- lapply(seq_along(synchronized.data), function(i){
  #   df <- synchronized.data[[i]]
  #   names(df)[2] <- paste0("temp", i)
  #   df
  # })

  synchronized.data <- imap(enhanced.data, \(x, idx) {
    x %>%
      transmute(
        datetime.UTC = floor_date(.data$datetime.UTC, "minute"),
        !!paste0("temp", idx) := .data$temp
      )
  }) %>%
    reduce(full_join, by = "datetime.UTC") %>%
    # mutate(date = as_date(.data$datetime),
    #        time = as_hms(.data$datetime),
    #        .after = .data$datetime
    # ) %>%
    add.datetime(timezone = timezone) |>
    mutate(
      temp = rowMeans(across(starts_with("temp")), na.rm = TRUE)
    )

  output <- list(
    # original.data = original.data,
    enhanced.data = enhanced.data,
    synchronized.data = synchronized.data
  )

  if (!is.null(agg.res)) {
    aggregated.data <- synchronized.data %>%
      mutate(datetime.UTC = floor_date(.data$datetime.UTC, agg.res)) %>%
      group_by(.data$datetime.UTC) %>%
      summarize(temp = mean(.data$temp, na.rm = TRUE)) %>%
      # mutate(date = as_date(.data$datetime),
      #        time = as_hms(.data$datetime),
      #        .after = .data$datetime
      # )
      add.datetime(timezone = timezone)

    output$aggregated.data <- aggregated.data
  }

  output
}
