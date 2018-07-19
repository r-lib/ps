
ps_env <- new.env(parent = emptyenv())

Internal <- NULL

## nocov start
.onLoad <- function(libname, pkgname) {
  ps_env$constants <- new.env(parent  = emptyenv())
  .Call(ps__init, asNamespace("ps"), ps_env$constants)
  if (!is.null(ps_env$constants$signals)) {
    ps_env$constants$signals <- as.list(ps_env$constants$signals)
  }
  if (!is.null(ps_env$constants$errno))  {
    ps_env$constants$errno <- as.list(ps_env$constants$errno)
  }

  Internal <<- get(".Internal", asNamespace("base"))

  ps_boot_time <<- memoize(ps_boot_time)
  get_terminal_map <<- memoize(get_terminal_map)
  NA_time <<- memoize(NA_time)
}
## nocov end

utils::globalVariables(c("self", "super"))
