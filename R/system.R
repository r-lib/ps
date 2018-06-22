
#' Ids of all processes on the system
#'
#' @return Integer vector of process ids.
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  if (os[["OSX"]])
    ps_pids_osx()
  else if (os[["LINUX"]])
    ps_pids_linux()
  else if (os[["WINDOWS"]])
    ps_pids_windows()
  else
    stop("Not implemented for this platform")
}

#' Check if a process id exists
#'
#' This is faster than calling [ps_pids()] first.
#'
#' @param pid Process id, integer scalar.
#' @return Logical scalar.
#'
#' @export

ps_pid_exists <- function(pid) {
  assert_that(is_pid(pid))
  os <- ps_os_type()
  if (os[["OSX"]])
    ps_pid_exists_osx(pid)
  else if (os[["LINUX"]])
    ps_pid_exists_linux(pid)
  else if (os[["WINDOWS"]])
    ps_pid_exists_windows(pid)
  else
    stop("Not implemented for this platform")
}

#' Boot time of the system
#'
#' @return A `POSIXct` object.
#'
#' @export

ps_boot_time <- function() {
  os <- ps_os_type()
  if (os[["LINUX"]])
    ps_boot_time_linux()
  else if (os[["WINDOWS"]])
    ps_boot_time_windows()
  else
    stop("Not implemented for this platform")
}
