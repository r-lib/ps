
#' Ids of all processes on the system
#'
#' @return Integer vector of process ids.
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  if (os[["MACOS"]])
    ps_pids_macos()
  else if (os[["LINUX"]])
    ps_pids_linux()
  else if (os[["WINDOWS"]])
    ps_pids_windows()
  else
    stop("Not implemented for this platform")
}

#' Boot time of the system
#'
#' @return A `POSIXct` object.
#'
#' @export

ps_boot_time <- function() {
  format_unix_time(ps_boot_time_raw())
}

ps_boot_time_raw <- function() {
  os <- ps_os_type()
  if (os[["LINUX"]])
    ps_boot_time_raw_linux()
  else if (os[["WINDOWS"]])
    ps_boot_time_raw_windows()
  else
    stop("Not implemented for this platform")
}

#' Parent proccesses for each process
#'
#' On Windows systems the parent process might not exist any more, and
#' its pid might have been reused, potentially.
#'
#' On POSIX systems, if the parent process exited already, usually PID 1
#' is shown instead of its id.
#'
#' @return A two-column data frame with integer columns `pid` and `ppid`.
#'
#' @export

ps_ppid_map <- function() {
  os <- ps_os_type()
  if (os[["WINDOWS"]]) {
    ps_ppid_map_windows()
  } else {
    pids <- ps_pids()
    processes <- not_null(lapply(pids, function(p) {
      tryCatch(ps_handle(p), no_such_process = function(e) NULL) }))
    pd <- map_int(processes, function(p) fallback(ps_pid(p), NA_integer_))
    pp <- map_int(processes, function(p) fallback(ps_ppid(p), NA_integer_))
    data.frame(
      pid = pd,
      ppid = pp
    )
  }
}
