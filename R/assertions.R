
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

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " must be a string (length 1 character)")
}
