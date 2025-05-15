#' Intertidal chamber multiday profile generation
#'
#' @param setup
#' @param start.date
#'
#' @returns
#' @export
#'
#' @examples
#' heatwave.setup <- data.frame(day = seq(0, 34),
#' mean.air.temp =  c(rep(17, 7), seq(17, 29, 2), rep(31, 7), seq(29, 17, -2), rep(17, 7) ),
#' mean.water.temp = c(rep(19, 7), seq(19, 25, 1), rep(26, 7), seq(25, 19, -1), rep(19, 7) ) )
#'
#' ic.multiday(heatwave.setup)
ic.multiday <- function(setup,
                     start.date = "2025-04-30",
                     export = FALSE
                     ){

  multiday.list <- do.call(mapply, c(ic.diurnal, setup, SIMPLIFY = FALSE))

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
    output.xlsx <- subset(output,
                          select = c(datetime, exp.temp, tide, light, wc, profile))

    output.txt <- output$profile

    write_xlsx(output.xlsx, "Profile.xlsx")

    write.table(output.txt, "Profile.txt",
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }


  output
}
