
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

#' @export
#' @rdname ps_os_type

ps_os_is_posix <- function() {
  ps_os_type()[["POSIX"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_windows <- function() {
  ps_os_type()[["WINDOWS"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_linux <- function() {
  ps_os_type()[["LINUX"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_macos <- function() {
  ps_os_type()[["MACOS"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_freebsd <- function() {
  ps_os_type()[["FREEBSD"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_openbsd <- function() {
  ps_os_type()[["OPENBSD"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_netbsd <- function() {
  ps_os_type()[["NETBSD"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_bsd <- function() {
  ps_os_type()[["BSD"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_sunos <- function() {
  ps_os_type()[["SUNOS"]]
}

#' @export
#' @rdname ps_os_type

ps_os_is_aix <- function() {
  ps_os_type()[["AIX"]]
}

ps_os_name <- function() {
  os <- ps_os_type()
  os <- os[setdiff(names(os), c("BSD", "POSIX"))]
  names(os)[which(os)]
}
