
ps_pids_osx <- function() {
  ls <- .Call(ps__pids)
  ## 0 is missing from the list, usually, even though it is a process
  if (! 0L %in% ls && ps_pid_exists(0L)) {
    ls <- c(ls, 0L)
  }
  ls
}

ps_pid_exists_osx <- function(pid) {
  .Call(ps__pid_exists, as.integer(pid))
}
