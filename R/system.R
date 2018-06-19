
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  if (os[["OSX"]])
    ps_pids_osx()
  else
    stop("Not implemented for this platform")
}

#' @export

ps_pid_exists <- function(pid) {
  assert_that(is_pid(pid))
  os <- ps_os_type()
  if (os[["OSX"]])
    ps_pid_exists_osx(pid)
  else
    stop("Not implemented for this platform")
}
