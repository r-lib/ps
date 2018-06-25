
ps__access_denied <- function() {
  structure(
    list(message = "Permission denied", errno = 0),
    class = c("access_denied", "ps_error", "error", "condition"))
}

ps__no_such_process <- function(pid = NULL, name = NULL) {
  pid <- pid %||% '???'
  name <- name %||% '???'
  structure(
    list(
      message = sprintf("No such process: %s (pid %i)", pid, name),
      errno = 0),
    class = c("no_such_process", "ps_error", "error", "condition"))
}
