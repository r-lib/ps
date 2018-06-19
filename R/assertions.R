
#' @importFrom assertthat assert_that on_failure<-

is_pid <- function(x) {
  is.numeric(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    round(x) == x &&
    x >= 0
}

on_failure(is_pid)  <- function (call, env) {
  paste0(deparse(call$x), " is not a valid process id")
}
