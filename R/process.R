
#' Create a system process object
#'
#' TODO
#'
#' @param pid  Process id, integer scalar.
#' @return `process` object.
#'
#' @export

process <- function(pid) {
  osname <- ps_os_name()
  if (is.na(osname)) stop("Unsupported platform")
  switch(
    osname,
    OSX = process_osx()$new(pid),
    LINUX = process_linux()$new(pid),
    WINDOWS = process_windows()$new(pid)
  )
}
