
ps__access_denied <- function(pid = NULL, name = NULL) {
  pid <- as.character(pid %||% "???")
  name <- name %||% "???"
  structure(
    list(message = sprintf("Permission denied: %s (pid %s)", name, pid),
         errno = 0),
    class = c("access_denied", "ps_error", "error", "condition"))
}

ps__no_such_process <- function(pid = NULL, name = NULL) {
  pid <- as.character(pid %||% "???")
  name <- name %||% "???"
  structure(
    list(
      message = sprintf("No such process: %s (pid %s)", name, pid),
      errno = 0),
    class = c("no_such_process", "ps_error", "error", "condition"))
}

ps__not_implemented <- function() {
  structure(list(
    message = "Not implemented on this platform", errno = 0),
    class = c("not_implemented", "ps_error", "error",  "condition"))
}

ps__zombie_process <- function(pid = NULL, name = NULL, ppid = NULL) {
  pid <- as.character(pid %||% "???")
  name <- name %||% "???"
  ppid <- as.character(ppid %||% "???")
  structure(
    list(message = sprintf("Zombie process: %s (pid %s, parent: %s)",
                           name, pid, ppid), errno = 0),
    class = c("zombie_process", "ps_error", "error", "condition"))
}

ps__package_not_available <- function(pkg, func = NULL) {
  structure(
    list(message = paste0(
      "Package `", pkg,  "` is not available",
      if (!is.null(func)) " for `", func, "()`")),
    class = c("package_not_available", "ps_error", "error", "condition"))
}
