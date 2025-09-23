#' Intertidal Chamber: Designing Multiday Profile
#'
#' @param setup
#' @param start.date
#' @param export
#' @param folder.path
#'
#' @returns
#' @export
#'
#' @examples
#' # acclimation phase
#' acclimation.setup <- data.frame(day = seq(0, 34),
#' mean.air.temp = 17, mean.water.temp = 19)
#'
#' acclimation.profile <- chamber.multiday(acclimation.setup,
#' start.date = "2025-05-15", export = FALSE)
#' ## to save the output files, change 'export' to 'TRUE'.
#'
#' acclimation.profile
#'
chamber.multiday <- function(setup,
                     start.date = "2025-04-30",
                     export = FALSE,
                     folder.path = NULL
                     ){

  multiday.list <- do.call(mapply, c(chamber.diurnal, setup, SIMPLIFY = FALSE))

  multiday.df <- do.call(rbind, multiday.list)

  output <- multiday.df %>%
    mutate(
      datetime = as.POSIXct(start.date, tz = "UTC") + day.dec * 24 * 60 * 60, # no. of seconds per day
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
    )

  # check
  invalid <- which(nchar(output$profile) != 19)
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

    output.xlsx <- subset(output,
                          select = c(datetime, exp.temp, tide, light, wc, profile))

    output.txt <- output$profile

    write_xlsx(output.xlsx, file.path(folder.path, "Profile.xlsx"))

    write.table(output.txt, file.path(folder.path, "Profile.txt"),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }

  # show input but with a new column for date
  setup$date <- as.POSIXct(start.date, tz = "UTC") + as.difftime(setup$day, units = "days")

  list(input = setup, output = output)
}
