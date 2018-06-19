
#' Create a system process object
#' @export

process <- function(pid) {
  os <- ps_os_type()
  if (os[["OSX"]])
    process_osx$new(pid)
  else
    stop("Not implemented for this platform")
}
