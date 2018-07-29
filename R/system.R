
#' Ids of all processes on the system
#'
#' @return Integer vector of process ids.
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  pp <- if (os[["MACOS"]])
    ps_pids_macos()
  else if (os[["LINUX"]])
    ps_pids_linux()
  else if (os[["WINDOWS"]])
    ps_pids_windows()
  else
    stop("Not implemented for this platform")

  sort(pp)
}

ps_pids_windows <- function() {
  sort(.Call(ps__pids))
}

ps_pids_macos <- function() {
  ls <- .Call(ps__pids)
  ## 0 is missing from the list, usually, even though it is a process
  if (! 0L %in% ls && ps_pid_exists_macos(0L)) {
    ls <- c(ls, 0L)
  }
  ls
}

ps_pid_exists_macos <- function(pid) {
  .Call(psp__pid_exists, as.integer(pid))
}

ps_pids_linux <- function() {
  sort(as.integer(dir("/proc", pattern = "^[0-9]+$")))
}

#' Boot time of the system
#'
#' @return A `POSIXct` object.
#'
#' @export

ps_boot_time <- function() {
  format_unix_time(.Call(ps__boot_time))
}

#' List users connected to the system
#'
#' @return A data frame (tibble) with columns
#'  `username`, `tty`, `hostname`, `start_time`, `pid`. `tty` and `pid`
#'  are `NA` on Windows. `pid` is the process id of the login process.
#'  For local users the `hostname` column is the empty string.
#'
#' @export

ps_users <- function() {
  l <- not_null(.Call(ps__users))

  d <- data.frame(
    stringsAsFactors = FALSE,
    username = vapply(l, "[[", character(1), 1),
    tty = vapply(l, "[[", character(1), 2),
    hostname = vapply(l, "[[", character(1), 3),
    start_time = format_unix_time(vapply(l, "[[", double(1), 4)),
    pid = vapply(l, "[[", integer(1),  5)
  )

  requireNamespace("tibble", quietly = TRUE)
  class(d) <- unique(c("tbl_df", "tbl", class(d)))
  d
}
