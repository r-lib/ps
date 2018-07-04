
#' Create a system process object
#'
#' TODO
#'
#' @param pid  Process id, integer scalar. If not specified or `NULL`,
#'   the pid of the  calling process is used.
#' @return `process` object.
#'
#' @export

process <- function(pid = NULL) {
  osname <- ps_os_name()
  if (is.na(osname)) stop("Unsupported platform")
  switch(
    osname,
    MACOS = process_macos()$new(pid),
    LINUX = process_linux()$new(pid),
    WINDOWS = process_windows()$new(pid)
  )
}

process_time <- function(pid = NULL, time = NULL) {
  osname <- ps_os_name()
  if (is.na(osname)) stop("Unsupported platform")
  switch(
    osname,
    MACOS = process_macos()$new(pid, time),
    LINUX = process_linux()$new(pid, time),
    WINDOWS = process_windows()$new(pid, time)
  )
}
