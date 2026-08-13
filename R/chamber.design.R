#' Chamber: Designing Multiday Profile
#'
#' A function to expand the diurnal patterns of environmental variables into multiday patterns.
#'
#' @param daily.settings a data frame, diurnal patterns of environmental variables. Columns must match arguments of [chamber.diurnal()].
#' @param start.date a character string, start date of the experiment. Time automatically set to 00:00:00.
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
#' as.POSIXct(acc$design$timestamp[1], tz = "UTC")
#' acc$design$datetime[1]
#' acc$daily.settings$date[1]
chamber.design <- function(daily.settings,
                     start.date = "2025-04-30",
                     timezone = "",
                     export = TRUE,
                     folder.path = NULL
                     ){

  # notice about time zone
  message("reminder: chamber interprets the UTC datetime of Unix timestamps in Profile.txt as local datetime")
  ### as.POSIXct(1747267200, tz = "UTC")
  ### e.g., 1747267200 is "2025-05-15 UTC" but will be interpreted as "2025-05-15 CEST"
  ### this is to allow a team in Germany and a team in China to run the same experimental profile at their local time

  message("assuming ", ifelse(timezone == "", Sys.timezone(), timezone), " as local time zone")

  # essentially, apply diurnal expansion to daily.settings to get expanded design
  multiday.list <- do.call(mapply, c(chamber.diurnal, daily.settings, SIMPLIFY = FALSE))

  multiday.df <- do.call(rbind, multiday.list)

  design <- multiday.df %>%
    mutate(
      # datetime = as.POSIXct(start.date, tz = "UTC") + as.difftime(day.dec, units = "days"), # how about not even calling day decimal
      datetime = as.POSIXct(start.date, tz = "UTC") + as.difftime(.data$day, units = "days") + as.difftime(.data$hour, units = "hours"),
      timestamp = as.numeric(.data$datetime), # chamber needs Unix timestamp (by definition always in UTC) so 'datetime' needs to be in UTC for now!!!
      profile = paste0(
        format(.data$timestamp, scientific = FALSE), # otherwise timestamp such as 1746000000 (2025-04-30 08:00:00) will become 1.746e+09
        "-",
        sprintf("%03d", .data$temp * 10), # decimal integer, 3 digits, leading 0
        .data$immersion,
        sprintf("%03d", .data$light),
        .data$wc
      )
    ) %>%
    mutate(datetime = force_tz(.data$datetime, tzone = timezone)) |> # force it back to local time as interpreted by chamber
    add.datetime(type = 2) # add date and time columns

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
  daily.settings$date <- as.POSIXct(start.date, tz = timezone) + as.difftime(daily.settings$day, units = "days") # chamber implements local time

  # export files
  if (export) {

    if (is.null(folder.path)) {
      folder.path <- getwd()
    }

    if (!dir.exists(folder.path)) {
      dir.create(folder.path, recursive = TRUE)
    }

    write.csv(
      if ("ie.cycle" %in% names(daily.settings)){
        # transform(daily.settings, ie.cycle = sapply(ie.cycle, toString)) #no visible binding for global variable 'ie.cycle'
        copy.daily.settings <- daily.settings
        copy.daily.settings$ie.cycle <- sapply(copy.daily.settings$ie.cycle, toString)
        copy.daily.settings
      } else {
        daily.settings
      },
              file.path(folder.path, "daily.settings.csv"),
              # quote = FALSE, # need to be TRUE to wrap c(0,1,0,1) in "" otherwise commas interfere in CSV
              row.names = FALSE)

    # write_xlsx(design.xlsx, file.path(folder.path, "Profile.xlsx")) # not use, as datetime column in excel shows UTC time!
    write.csv(design[, c("datetime", "temp", "immersion", "light", "wc", "profile")], # dont need subset with .data$datetime
              file.path(folder.path, "design.csv"),
              # quote = FALSE,
              row.names = FALSE)

    write.table(design$profile,
                file.path(folder.path, "Profile.txt"),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }

  class(design) <- c("chamber.design", "data.frame")

  list(daily.settings = daily.settings, design = design)
}
