
#' Start a process in the background
#'
#' Start a process in the background with [processx::process], and return
#' an object that supports both the `processx::process` and `ps::process`
#' methods.
#'
#' @param command Character scalar, the command to run. Note that
#'   this argument is not passed to a shell, so no tilde-expansion
#'   or variable substitution is performed on it. It should not be
#'   quoted with ‘base::shQuote()’. See ‘base::normalizePath()’
#'   for tilde-expansion.
#' @param args Character vector, arguments to the command. They will
#'   be used as is, without a shell. They don't need to be
#'   escaped.
#' @param ... Additional arguments are passed to [processx::process].
#'
#' @section Examples:
#' ```
#' p <- ps_start("sleep", "5")
#' p
#'
#' ## processx methods
#' p$wait(100)
#' p$poll_io(100)
#'
#' ## ps methods
#' p$suspend()
#' p$resume()
#' ```
#'
#' @export

ps_start <- function(command, args = character(), ...) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop(ps__package_not_available("processx", "ps::ps_start"))
  }

  if (packageVersion("processx") < "3.1.0.9005") {
    stop(ps__package_not_available("processx >= 3.1.0.9005", "ps::ps_start"))
  }

  prp <- processx::process$new(command, args, ...)
  psp <- process_time(prp$get_pid(), as.numeric(prp$get_start_time()))

  env <- new.env(parent = emptyenv())

  for (n in ls(prp, all.names = TRUE)) env[[n]] <- prp[[n]]
  for (n in ls(psp, all.names = TRUE)) env[[n]] <- psp[[n]]

  env$.__enclos_env__ <- prp$.__enclos_env__

  class(env) <- c(setdiff(class(prp), "R6"), class(psp))
  env
}
