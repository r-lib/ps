
ps__access_denied <- function(pid = NULL, name = NULL) {
  pid <- pid %||% "???"
  name <- name %||% "???"
  structure(
    list(message = sprintf("Permission denied: %s (pid %i)", name, pid),
         errno = 0),
    class = c("access_denied", "ps_error", "error", "condition"))
}

ps__no_such_process <- function(pid = NULL, name = NULL) {
  pid <- pid %||% "???"
  name <- name %||% "???"
  structure(
    list(
      message = sprintf("No such process: %s (pid %i)", name, pid),
      errno = 0),
    class = c("no_such_process", "ps_error", "error", "condition"))
}

ps__not_implemented <- function() {
  structure(list(
    message = "Not implemented on this platform", errno = 0),
    class = c("not_implemented", "ps_error", "error",  "condition"))
}
