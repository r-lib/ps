
#' @export

ps_pids <- function() {
  ls <- .Call(ps__pids)
  if (! 0L %in% ls && ps_pid_exists(0L)) {
    ls <- c(ls, 0L)
  }
  ls
}

#' @export

ps_pid_exists <- function(pid) {
  assert_that(is_pid(pid))
  .Call(ps__pid_exists, as.integer(pid))
}
