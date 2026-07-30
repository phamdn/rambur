#' Chamber: Designing Multiday Profile
#'
#' A function to expand the diurnal patterns of environmental variables into multiday patterns.
#'
#' @param daily.settings a data frame, diurnal patterns of environmental variables. Columns must match arguments of [chamber.diurnal()].
#' @param start.date a character string, start date of the experiment.
#' @param export a logical, whether to export the output as files (e.g., Profile.txt).
#' @param folder.path a character string, path to the export folder.
#' @param timezone a character string, time zone.
#'
#' @returns a list of two data frames, \code{daily.settings} as input but with an extra column \code{date}, and \code{design} of expanded multiday patterns.
#' @export
#'
#' @seealso [chamber.diurnal()]
#'
#' @examples
#' # acclimation phase
#' acc.daily.settings <- data.frame(day = seq(0, 34),
#' temp.air.mean = 17, temp.water.mean = 19)
#'
#' acc <- chamber.design(acc.daily.settings,
#' start.date = "2025-05-15", export = FALSE)
#' ## to save the output files, set 'export' to 'TRUE'.
#'
#' acc
#'
chamber.design <- function(daily.settings,
                     start.date = "2025-04-30",
                     timezone = "",
                     export = FALSE,
                     folder.path = NULL
                     ){

  # notice about time zone
  if (timezone == "") {
    message("using ", Sys.timezone(), " time zone")
  }

  multiday.list <- do.call(mapply, c(chamber.diurnal, daily.settings, SIMPLIFY = FALSE))

  multiday.df <- do.call(rbind, multiday.list)

  # chamber use UTC timestamp but implement it as local time
  design <- multiday.df %>%
    mutate(
      # datetime = as.POSIXct(start.date, tz = "UTC") + day.dec * 24 * 60 * 60, # no. of seconds per day, how about as.difftime()
      # datetime = as.POSIXct(start.date, tz = "UTC") + as.difftime(day.dec, units = "days"), # how about not even calling day decimal
      datetime = as.POSIXct(start.date, tz = "UTC") + as.difftime(.data$day, units = "days") + as.difftime(.data$hour, units = "hours"),
      timestamp = as.numeric(.data$datetime), # chamber uses UTC timestamp!!!
      profile = paste0(
        format(.data$timestamp, scientific = FALSE),
        # otherwise timestamp such as 1746000000 (2025-04-30 08:00:00) will become 1.746e+09
        "-",
        sprintf("%03d", .data$temp * 10), # decimal integer, 3 digits, leading 0
        .data$immersion,
        sprintf("%03d", .data$light),
        .data$wc
      )
    ) %>%
    # mutate(datetime = force_tz(.data$datetime, tzone = timezone), # chamber uses UTC timestamp but implements it as local time
    #        date = as_date(.data$datetime), # better than base R as.Date() in preserving correct time zone
    #        time = as_hms(.data$datetime),
    #        .after = .data$datetime
    #        )
    mutate(datetime = force_tz(.data$datetime, tzone = timezone)) |> # chamber uses UTC timestamp but implements it as local time
    add.datetime(timezone = timezone)

  # check profile: length of each line
  invalid <- which(nchar(design$profile) != 19)
  if (length(invalid) > 0) {
    warning("Line(s) ", paste(invalid, collapse = " "), " of the profile are not 19 characters long")
  }

  # check profile: no. of lines
  if (nrow(design) > 1000) {
    warning("The profile has ", nrow(design), " lines, which exceeds the 1000-line limit (firmware v8.09). Please use chamber.split()")
  }

  # return input but with a new column for date, should not use <<- to change the input globally
  daily.settings$date <- as.POSIXct(start.date, tz = timezone) + as.difftime(daily.settings$day, units = "days")
  # chamber use UTC timestamp but implement it as local time

  # export
  if (export) {

    if (is.null(folder.path)) {
      folder.path <- getwd()
    }

    if (!dir.exists(folder.path)) {
      dir.create(folder.path, recursive = TRUE)
    }

    write.csv(transform(daily.settings, ie.cycle = sapply(ie.cycle, toString)),
              file.path(folder.path, "daily.settings.csv"),
              # quote = FALSE, # need to be TRUE otherwise wrong cols due to c(0,1,0,1)
              row.names = FALSE)

    # design.csv <- subset(design,
    #                       select = c(.data$datetime, .data$temp, .data$tide, .data$light, .data$wc, .data$profile))
    design.csv <- design[, c("datetime", "temp", "immersion", "light", "wc", "profile")]

    Profile.txt <- design$profile

    # write_xlsx(design.xlsx, file.path(folder.path, "Profile.xlsx")) # datetime column in excel shows UTC time!
    write.csv(design.csv, file.path(folder.path, "design.csv"),
              # quote = FALSE,
              row.names = FALSE)

    write.table(Profile.txt, file.path(folder.path, "Profile.txt"),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }

  # essentially, apply diurnal expansion to daily.settings to get expanded design
  list(daily.settings = daily.settings, design = design)
}
