#' Robomussels: Reading Multiple CSV Records
#'
#' @param folder.path
#' @param file.name
#' @param metadata.lines
#' @param timezone
#' @param summary.period
#'
#' @returns
#' @export
#'
#' @examples
#' robo.folder <- system.file("extdata/robo", package = "rambur")
#' robo.data2 <- robo.read2(robo.folder)
#' robo.data2
robo.read2 <- function(folder.path = NULL,
                       file.name = ".csv",
                      metadata.lines = 21,
                      timezone = "",
                      summary.period = "hour"){

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
      transmute(datetime.UTC = time,
                # datetime = format(time, tz = timezone), not working, just <chr> format
                datetime = as.POSIXct(time, tz = timezone),
                date = as_date(datetime),
                time = as_hms(datetime), # as.Date is base R but as_date and as_hms is not
                body.temp = temp
      ) # transmute() is better than mutate() for keeping columns in desired order, note the repurposed use of "time"
  })

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

  combined.data <- imap(enhanced.data, \(x, idx) {
    x %>%
      transmute(
        datetime = floor_date(datetime, "minute"),
        !!paste0("body.temp", idx) := body.temp
      )
  }) %>%
    reduce(full_join, by = "datetime") %>%
    mutate(date = as_date(datetime),
           time = as_hms(datetime),
           .after = datetime
    ) %>%
    mutate(
      body.temp = rowMeans(across(starts_with("body.temp")), na.rm = TRUE)
    )


  # combined.data <- imap(synchronized.data, ~ {
  #   rename(.x, !!paste0("body.temp", .y) := body.temp)
  # }) %>%
  #   reduce(full_join, by = "datetime")




  summarized.data <- combined.data %>%
    mutate(datetime = floor_date(datetime, summary.period)) %>%
    group_by(datetime) %>%
    summarize(body.temp = mean(body.temp, na.rm = TRUE)) %>%
    mutate(date = as_date(datetime),
           time = as_hms(datetime),
           .after = datetime
    )

  list(
    # original.data = original.data,
       enhanced.data = enhanced.data,
       combined.data = combined.data,
       summarized.data = summarized.data
  )

}
