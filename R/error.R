
ps__package_not_available <- function(pkg, func = NULL) {
  structure(
    list(message = paste0(
      "Package `", pkg,  "` is not available",
      if (!is.null(func)) " for `", func, "()`")),
    class = c("package_not_available", "ps_error", "error", "condition"))
}
