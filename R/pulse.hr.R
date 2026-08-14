#' Pulse: Calculating Heart Rate
#'
#' A worker function to calculate heart rate
#'
#' @param signal an integer vector, infrared signal from pulse device.
#' @param sampling.rate an integer, sampling rate in Hz.
#' @param cor.min a numeric, the correlation threshold for qualified signal.
#' @param score.parameter a numeric, the exponent used in the score.
#' @param display a character string, whether to display plots.
#' @param search.scope a numeric, the ratio of lag domain length to the total signal length.
#' @param ppg.zoom a logical, whether to zoom the PPG plot to fit signal intensity.
#'
#' @returns a mixed list, \code{locmax} for local maxima, \code{nominee} for the best peak, \code{cor.pass} and \code{anticor.pass} for quality control, and \code{hr} for final heart rate.
#' @export
#'
#' @seealso [pulse.extract()]
#'
#' @examples
#' folder <- system.file("extdata/pulse", package = "rambur")
#' pulse.data <- pulse.read(folder)
#' ex1 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:01:00" &
#' datetime <= "2025-05-21 00:02:00", channel.1, drop = TRUE))
#' ex2 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:09:00" &
#' datetime <= "2025-05-21 00:10:00", channel.3, drop = TRUE), display = "none")
#' ex3 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:30:00" &
#' datetime <= "2025-05-21 00:31:00", channel.10, drop = TRUE), display = "ppg")
pulse.hr <- function(signal,
                     sampling.rate = 5,
                     search.scope = 1,
                     score.parameter = 0.5,
                     cor.min = 0.5,
                     display = c("all", "ppg", "none"),
                     ppg.zoom = FALSE
){
  signal.length <- length(signal)
  lag.max <- signal.length * search.scope # consider shortening for faster computation
  timelag.max <- lag.max / sampling.rate # get xlim for plot

  # autocorrelation
  ac.list <- acf(signal, lag.max = lag.max, plot = FALSE) # will plot manually

  ac <- data.frame(lag = ac.list$lag,
                   cor = ac.list$acf) # note that cor = 1 at lag = 0

  # find all discrete local maxima (acf peaks)
  locmax.idx <- which(diff(sign(diff(ac$cor))) == -2) + 1
  locmax.idx <- locmax.idx[ac$cor[locmax.idx] > 0] # retain only the positive ones
  # locmax.discrete <- ac[locmax.idx, ] # save to view, no further calculations based on it

  # quadratic interpolation from discrete locmax to estimate true fractional locmax
  # cor before, at, and after the discrete locmax
  y1 <- ac$cor[locmax.idx - 1] # vectorized
  y2 <- ac$cor[locmax.idx]
  y3 <- ac$cor[locmax.idx + 1]
  # the shift of lag
  p <- 0.5 * (y1 - y3) / (y1 - 2 * y2 + y3)
  # interpolated locmax
  locmax <- data.frame(
    lag = ac$lag[locmax.idx] + p, # lag adjusted, can also use locmax.discrete$lag + p
    cor = y2 - 0.25 * (y1 - y3) * p # cor of true locmax
  )

  # convert lag to time lag in seconds
  locmax$timelag <- locmax$lag / sampling.rate

  # compute nomination score using power-law decay
  locmax$score <- locmax$cor / (locmax$timelag)^score.parameter

  # two peaks with highest scores, retain temporal order using sort()

  if(nrow(locmax) < 2) { # edge case when signal is too short, there might be less than 2 locmax
    top2 <- locmax[c(NA_integer_, NA_integer_), ]
  } else {
    top2 <- locmax[sort(order(locmax$score, decreasing = TRUE)[1:2]), ]
  }

  top2$role <- c("Nominee", "Validator")

  top2$lag.ratio <- top2$lag / top2$lag[1] # lag of validator over nominee

  # compute hr in beats per minute (bpm) for top2
  top2$hr <- 60 / top2$timelag

  # quality checks
  # place holders
  strength.pass <- decay.pass <- echo.pass <- anticor.pass <- hr <- NA

  # strength
  strength.pass <- top2$cor[1] >= cor.min

  # decay
  if (isTRUE(strength.pass)){
    decay.pass <- top2$cor[1] > top2$cor[2]
  }

  # echo
  if (isTRUE(decay.pass)){
    echo.pass <- round(top2$lag.ratio[2], digits = 1) == 2 #round(seq(1.94, 2.06, 0.01), digits = 1)
  }

  # anticor
  if (isTRUE(echo.pass)){
    anticor.pass <- any(ac$cor[ac$lag < top2$lag[1]] < 0)
  }

  if (isTRUE(strength.pass &&
             decay.pass &&
             echo.pass &&
             anticor.pass
             )) {
    hr <- top2$hr[1]
  }

  quality <- c(strength = strength.pass,
               decay = decay.pass,
               echo = echo.pass,
               anticor = anticor.pass)

  # # best candidate
  # nominee <- top2[1, ]
  #
  # if(nrow(nominee) == 0) { # just in case nominee is empty
  #   nominee <- locmax[NA_integer_, ]
  #   cor.pass <- anticor.pass <- hr <- NA
  # } else {
  #   # quality check for cor
  #   cor.pass <- nominee$cor >= cor.min
  #
  #   # quality check for anti-correlation: trivial rhythms, no negative correlation occur before the dominant peak
  #   segment <- ac[ac$lag < nominee$lag, ] # segment preceding the dominant locmax/peak, note that subset() causes no visible binding for global variable issue
  #   anticor.pass <- any(segment$cor < 0)
  #
  #   # final hr
  #   if (cor.pass && anticor.pass) {
  #     hr <- nominee$hr
  #   }
  #   else hr <- NA
  # }

  # output list
  output <- list(
    # ac.list = ac.list,
    # ac = ac,
    # locmax.discrete = locmax.discrete,
    locmax = locmax,
    top2 = top2,
    # nominee = nominee,
    quality = quality,
    hr = hr)

  # display
  display <- match.arg(display)

  if (display == "ppg") {
    plot(signal, type = "l", main = "Photoplethysmogram", ylab = "Intensity",
         ylim = if (ppg.zoom) NULL else c(0, 4095)
         )
    text(x = signal.length, y = if (ppg.zoom) min(signal) else 0,
         labels = paste(signal.length / sampling.rate, "s\n", sampling.rate, "Hz"),
         adj = c(1, 0), col = 2)
  }

  # if (display %in% c("all", "baseR")) { # hide these if human counting
  #   plot(ac.list, main = "Autocorrelogram", ylab = "Correlation", ci = 0, col = 8)
  #   points(locmax$lag, locmax$cor, pch = 19, col = 4)
  #   legend("topright", legend = "Interpolated peaks", pch = 19, col = 4)
  #
  #   plot(locmax$timelag, locmax$cor, type = "o", pch = 19, lty = 5, col = 4,
  #        main = "Peak evaluation", xlab = "Time lag (s)", ylab = "",
  #        xlim = c(0, timelag.max), ylim = c(0, 1))
  #   lines(locmax$timelag, locmax$score, type = "o", pch = 19, lty = 5, col = 2) # plot score
  #   points(top2$timelag[1], top2$cor[1], col = 2, cex = 3) # nominee
  #   points(top2$timelag[2], top2$cor[2], col = 1, cex = 3) # Validator
  #   text(timelag.max, 0.8,
  #        labels = ifelse(is.na(hr), "HR = N/A", paste("HR =", round(hr, 1), "bpm")),
  #        pos = 2, col = 2)
  #   abline(h = cor.min, col = 4, lty = 2)
  #   legend("topright", legend = c("Score", "Nominee", "Validator"),
  #          col = c(2, 2, 1), text.col = 1, pch = c(19, 1, 1), lty = c(5, NA, NA))
  #
  #   print(output)
  # }

  if (display == "all") {

    fig1 <- data.frame(Index = seq_along(signal),
                       Intensity = signal) |>
    ggplot(aes(x = .data$Index, y = .data$Intensity)) +
      geom_line() +
      annotate("text", x = signal.length, y = if (ppg.zoom) min(signal) else 0,
               label = paste(signal.length / sampling.rate, "s \u00d7", sampling.rate, "Hz"),
                col = 2, hjust = 0.75, vjust = 0
               ) +
      scale_y_continuous(limits = if (ppg.zoom) NULL else c(0, 4095)) +
      labs(title = "Photoplethysmogram") +
      theme_cowplot()

    fig2 <- ac |> ggplot(aes(x = .data$lag, y = .data$cor)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(aes(xend = .data$lag, yend = 0), col = 8) +
      geom_point(data = locmax, aes(color = "Interpolated peaks")) +
      scale_color_manual(values = c("Interpolated peaks" = 4)) +
      labs(title = "Autocorrelogram", y = "Correlation", x = "Lag", color = NULL) +
      theme_cowplot() +
      theme(legend.position = "top")

    fig3 <- locmax |> ggplot(aes(x = .data$timelag)) +
      {if (cor.min != 0)
        geom_hline(yintercept = cor.min, color = 4)
      } +
      geom_line(aes(y = .data$cor), color = 4, linetype = 3) +
      geom_point(aes(y = .data$cor), color = 4) +
      {if (score.parameter != 0) list(
        geom_line(aes(y = .data$score, color = "Score"), linetype = 3),
          geom_point(aes(y = .data$score, color = "Score"))
      )
        } +
      # geom_point(data = nominee, aes(y = .data$score), shape = 1, size = 5, color = 2) +
      geom_point(data = top2, aes(y = .data$cor, color = .data$role), shape = 1, size = 6) +
      annotate("text", x = Inf, y = Inf, #x = timelag.max, y = 1,
               label = ifelse(is.na(hr), "HR = N/A", paste("HR =", round(hr, 1), "bpm")),
               col = 2, hjust = 1, vjust = 1
      ) +
      scale_x_continuous(limits = c(0, timelag.max)) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_color_manual(values = c("Score" = 2, "Nominee" = 2, "Validator" = 1),
                         breaks = c("Score", "Nominee", "Validator")
                         ) +
      labs(title = "Peak evaluation", x = "Time lag (s)", y = NULL, color = NULL) +
      theme_cowplot() +
      theme(legend.position = "top")

    # fig23 <- plot_grid(fig2, fig3, nrow = 1, align = "h") # cowplot
    # fig <- plot_grid(fig1, fig23, ncol = 1, axis = "l")

    fig <- fig1 / (fig2 | fig3) # patchwork syntax
    output$fig <- fig

    print(output)
  }

  output
}
