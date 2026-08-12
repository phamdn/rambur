#' Pulse: Calculating Heart Rate
#'
#' A worker function to calculate heart rate
#'
#' @param signal an integer vector, infrared signal from pulse device.
#' @param sampling.rate an integer, sampling rate in Hz.
#' @param cor.min a numeric, the correlation threshold for qualified signal.
#' @param score.parameter a numeric, the exponent used in the score.
#' @param display a character string, whether to plot display.
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
#' ex1b <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:01:00" &
#' datetime <= "2025-05-21 00:02:00", channel.1, drop = TRUE), display = "ggplot2")
#' ex2 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 01:07:00" &
#' datetime <= "2025-05-21 01:08:00", channel.1, drop = TRUE))
#' ex3 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:09:00" &
#' datetime <= "2025-05-21 00:10:00", channel.3, drop = TRUE))
#' ex4 <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:30:00" &
#' datetime <= "2025-05-21 00:31:00", channel.10, drop = TRUE), display = "baseR")
#' ex4b <- pulse.hr(subset(pulse.data, datetime >= "2025-05-21 00:30:00" &
#' datetime <= "2025-05-21 00:31:00", channel.10, drop = TRUE), display = "ggplot2")
pulse.hr <- function(signal,
                     sampling.rate = 5,
                     score.parameter = 0.5,
                     cor.min = 0.5,
                     display = c("all", "ppg", "none", "baseR", "ggplot2")
){
  # signal.length <- NROW(signal) # allow 1 column matrix or dataframe but not helpful as plotting functions need vector anyway
  signal.length <- length(signal)

  # autocorrelation
  lag.max <- signal.length / 2 # half of the signal length, e.g., 1-min signal has min detectable hr of 2 bpm
  timelag.max <- lag.max / sampling.rate # xlim for plotting

  ac.list <- acf(signal,
                 lag.max = lag.max,
                 plot = FALSE # will plot manually below
  )

  ac <- data.frame(lag = ac.list$lag,
                   cor = ac.list$acf) # note that cor = 1 at lag = 0

  # find all discrete local maxima (acf peaks)
  locmax.idx <- which(diff(sign(diff(ac$cor))) == -2) + 1
  locmax.idx <- locmax.idx[ac$cor[locmax.idx] > 0] # retain only the positive
  locmax.discrete <- ac[locmax.idx, ]

  # quadratic interpolation from discrete locmax to estimate true fractional locmax
  # cor before, at, and after the discrete locmax
  y1 <- ac$cor[locmax.idx - 1] # vectorized
  y2 <- ac$cor[locmax.idx]
  y3 <- ac$cor[locmax.idx + 1]
  # the shift of lag
  p <- 0.5 * (y1 - y3) / (y1 - 2 * y2 + y3)
  # interpolated locmax
  locmax <- data.frame(
    lag = ac$lag[locmax.idx] + p, # lag adjusted
    cor = y2 - 0.25 * (y1 - y3) * p # cor of true locmax
  )

  # convert lag to time in seconds
  locmax$timelag <- locmax$lag / sampling.rate

  # compute score using power-law decay
  locmax$score <- locmax$cor / (locmax$timelag)^score.parameter

  # score.method <- match.arg(score.method)
  # if (score.method == "power.law") {
    # locmax$score <- locmax$cor / (locmax$timelag)^score.parameter
  # } else if (score.method == "exponential") {
  #   locmax$score <- locmax$cor * exp(- score.parameter * locmax$timelag)
  # }

  # compute hr
  locmax$hr <- 60 / locmax$timelag # beats per minute (bpm)

  # best candidate
  nominee <- locmax[which.max(locmax$score), ]

  if(nrow(nominee) == 0) { # just in case nominee is empty
    nominee <- locmax[NA_integer_, ]
    cor.pass <- anticor.pass <- hr <- NA
  } else {
    # quality check for cor
    cor.pass <- nominee$cor >= cor.min

    # quality check for anti-correlation: trivial rhythms, no negative correlation occur before the dominant peak
    segment <- ac[ac$lag < nominee$lag, ] # segment preceding the dominant locmax/peak, note that subset() causes no visible binding for global variable issue
    anticor.pass <- any(segment$cor < 0)

    # final hr
    if (cor.pass && anticor.pass) {
      hr <- nominee$hr
    }
    else hr <- NA
  }

  quality <- c(cor = cor.pass, anticor = anticor.pass)

  # output list
  output <- list(
    # ac.list = ac.list,
    # ac = ac,
    locmax.discrete = locmax.discrete,
    locmax = locmax,
    nominee = nominee,
    quality = quality,
    hr = hr)

  # display
  display <- match.arg(display)

  if (display %in% c("all", "baseR", "ppg")) {
    plot(signal, type = "l", main = "Photoplethysmogram", ylab = "Intensity",
         ylim = c(0, 4095)
         )
    text(signal.length, 0, labels = paste(signal.length / sampling.rate, "s\n", sampling.rate, "Hz"),
         adj = c(1, 0), col = 2)
  }

  if (display %in% c("all", "baseR")) { # hide these if human counting
    plot(ac.list, main = "Autocorrelogram", ci = 0, col = 8)
    points(locmax$lag, locmax$cor, pch = 19)
    legend("topright", legend = "Interpolated peaks", pch = 19)

    plot(locmax$timelag, locmax$cor, type = "o", pch = 19, lty = 5,
         main = "Candidate scoring", xlab = "Time lag (s)", ylab = "",
         xlim = c(0, timelag.max), ylim = c(0, 1))
    lines(locmax$timelag, locmax$score, type = "o", pch = 19, lty = 5, col = 2) # plot score
    points(nominee$timelag, nominee$score, col = 2, cex = 3) # dominant highlight
    text(nominee$timelag, nominee$score, labels = paste(round(nominee$hr, 1), "bpm"), #"HR =",
         pos = 3, offset = 1, col = 2)
    abline(h = cor.min, col = 4, lty = 2)
    legend("topright", legend = c("Correlation", "Score"),
           col = 1:2, text.col = 1:2, pch = 19, lty = 5)

    print(output)
  }

  if (display == "ggplot2") {

    fig1 <- data.frame(Index = seq_along(signal),
                       Intensity = signal) |>
    ggplot(aes(x = .data$Index, y = .data$Intensity)) +
      geom_line() +
      annotate("text", x = signal.length, y = 0,
               label = paste(signal.length / sampling.rate, "s \u00d7", sampling.rate, "Hz"),
                col = 2,
               hjust = 0.75, vjust = 0
               ) +
      scale_y_continuous(limits = c(0, 4095)) +
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
      geom_line(aes(y = .data$cor, color = "Correlation"), linetype = 3) +
      geom_point(aes(y = .data$cor, color = "Correlation")) +
      {if (score.parameter != 0) list(
        geom_line(aes(y = .data$score, color = "Score"), linetype = 3),
          geom_point(aes(y = .data$score, color = "Score"))
      )
        } +
      geom_point(data = nominee, aes(y = .data$score), shape = 1, size = 5, color = 2) +
      annotate("text", x = Inf, y = Inf, #x = timelag.max, y = 1,
               label = ifelse(is.na(hr), "HR = N/A", paste("HR =", round(hr, 1), "bpm")),
               col = 2, hjust = 1, vjust = 1
      ) +
      scale_x_continuous(limits = c(0, timelag.max)) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_color_manual(values = c("Correlation" = 4,
                                    "Score" = 2)) +
      labs(title = "Candidate scoring", x = "Time lag (s)", y = NULL, color = NULL) +
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
