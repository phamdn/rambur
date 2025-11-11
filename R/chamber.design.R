#' Chamber: Designing Multiday Profile
#'
#' @param daily.settings
#' @param start.date
#' @param export
#' @param folder.path
#'
#' @returns
#' @export
#'
#' @examples
#' # acclimation phase
#' acc.daily.settings <- data.frame(day = seq(0, 34),
#' mean.air.temp = 17, mean.water.temp = 19)
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
      datetime = as.POSIXct(start.date, tz = "UTC") + as.difftime(day, units = "days") + as.difftime(hour, units = "hours"),
      timestamp = as.numeric(datetime),
      profile = paste0(
        format(timestamp, scientific = FALSE),
        # otherwise timestamp such as 1746000000 (2025-04-30 08:00:00) will become 1.746e+09
        "-",
        sprintf("%03d", exp.temp * 10), # decimal integer, 3 digits, leading 0
        tide,
        sprintf("%03d", light),
        wc
      )
    ) %>%
    mutate(datetime = force_tz(datetime, tzone = timezone), # chamber uses UTC timestamp but implements it as local time
           date = as_date(datetime),
           time = as_hms(datetime),
           .after = datetime
           )

  # check
  invalid <- which(nchar(design$profile) != 19)
  if (length(invalid) > 0) {
    warning(
      paste("Line(s)", paste(invalid, collapse = " "),
            "of 'profile' are not 19 characters long"
      )
    )
  }

  # export
  if (export) {

    if (is.null(folder.path)) {
      folder.path <- getwd()
    }

    if (!dir.exists(folder.path)) {
      dir.create(folder.path, recursive = TRUE)
    }

    design.xlsx <- subset(design,
                          select = c(datetime, exp.temp, tide, light, wc, profile))

    design.txt <- design$profile

    write_xlsx(design.xlsx, file.path(folder.path, "Profile.xlsx"))

    write.table(design.txt, file.path(folder.path, "Profile.txt"),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }

  # return input but with a new column for date, maybe should not use <<- to change the input globally
  daily.settings$date <- as.POSIXct(start.date, tz = timezone) + as.difftime(daily.settings$day, units = "days")
                # chamber use UTC timestamp but implement it as local time

  # apply diurnal expansion to daily.settings to get expanded design
  list(daily.settings = daily.settings, design = design)
}
