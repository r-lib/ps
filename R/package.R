
ps_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  ps_env$constants <- new.env(parent  = emptyenv())
  .Call(ps__init, asNamespace("ps"), ps_env$constants)
  if (!is.null(ps_env$constants$signals)) {
    ps_env$constants$signals <- as.list(ps_env$constants$signals)
  }
  if (!is.null(ps_env$constants$errno))  {
    ps_env$constants$errno <- as.list(ps_env$constants$errno)
  }
  get_terminal_map <<- memoize(get_terminal_map)
  NA_time <<- memoize(NA_time)
}

utils::globalVariables(c("self", "super"))
