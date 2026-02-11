#' @param file.path
#'
#' @param max.lines
#' @param overlap.lines
#' @param folder.path
#'
#' @export
chamber.split <- function(file.path,
                          max.lines = 960,
                          overlap.lines = 120,
                          folder.path = NULL){

  if (is.null(folder.path)) {
    folder.path <- dirname(file.path)
  }

  profile <- readLines(file.path)
  total.lines <- length(profile)

  start.lines <- seq(from = 1, to = total.lines, by = max.lines - overlap.lines)
  end.lines <- start.lines + max.lines - 1
  end.lines[end.lines > total.lines] <- total.lines

  message("splitting '", basename(file.path), "' with ", total.lines, " lines into:")

  for (i in 1:length(start.lines)){

    start.line <- start.lines[i]
    end.line <- end.lines[i]

    sub.profile <- profile[start.line:end.line]

    start.time <- head(sub.profile, 1) %>% substr(1, 10) %>% as.numeric %>% as.POSIXct(tz = "UTC") %>% format("%Y%m%d_%H%M")
    end.time <- tail(sub.profile, 1) %>% substr(1, 10) %>% as.numeric %>% as.POSIXct(tz = "UTC") %>% format("%Y%m%d_%H%M")

    # sub.folder.path <- file.path(folder.path, sprintf("part-%02d", i))
    sub.folder.path <- file.path(folder.path, paste0(start.time, "_to_", end.time)) #more informative sub folder names

    if (!dir.exists(sub.folder.path)) {
      dir.create(sub.folder.path, recursive = TRUE)
    }

    write.table(sub.profile, file.path(sub.folder.path, "Profile.txt"),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)

    message(sprintf("%s/Profile.txt (lines %d - %d)",
                    sub.folder.path, start.line, end.line))

  }


}
