
#' Ids of all processes on the system
#'
#' @return Integer vector of process ids.
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  if (os[["OSX"]])
    ps_pids_osx()
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
  else
    stop("Not implemented for this platform")
}
