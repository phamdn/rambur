#' RobomusselS: Reading Log Files
#'
#' A function to read the CSV log files of multiple robomussels or temperature EnvLoggers.
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
#' robos.read(robo.folder, agg.res = "1 hour")
robos.read <- function(folder.path = NULL,
                       file.name = ".csv",
                      metadata.lines = 21,
                      timezone = "",
                      agg.res = NULL){

  # notice about time zone
  message("reminder: robo log files were in UTC")

  if (is.null(folder.path)) {
    folder.path <- getwd()
    message("reading from the current working directory")
  }

  # get a list of all CSV files
  robo.files <- list.files(path = folder.path, pattern = file.name, full.names = TRUE)

  message("importing ", length(robo.files), " files")

  original.data <- lapply(robo.files, function(x){
     read_csv(file = x, skip = metadata.lines, show_col_types = FALSE)
    # read_csv uses UTC as default (see col_datetime() and locale()), which is the correct tz of robomussel (always UTC+0000)
  })

  enhanced.data <- lapply(original.data, function(x){
    x %>%
      # mutate(
      #   datetime.UTC = .data$time,
      #   .keep = "unused", .before = 1
      # ) |> # transmute() might be better than mutate() for keeping columns in desired order
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
    add.datetime(timezone = timezone) |>
    mutate(
      temp = rowMeans(across(starts_with("temp")))
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
      summarize(temp = mean(.data$temp)) %>%
      add.datetime(timezone = timezone)

    output$aggregated.data <- aggregated.data
  }

  output
}
