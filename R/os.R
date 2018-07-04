
#' Query the type of the OS
#'
#' @return `ps_os_type` return a named logical vector. The rest of the
#' functions return a logical scalar.
#'
#' @export

ps_os_type <- function() {
  if (is.null(ps_env$os_type)) ps_env$os_type <- .Call(ps__os_type)
  ps_env$os_type
}

ps_os_name <- function() {
  os <- ps_os_type()
  os <- os[setdiff(names(os), c("BSD", "POSIX"))]
  names(os)[which(os)]
}
