
ps_pids_windows <- function() {
  sort(.Call(ps__pids))
}

ps_boot_time_raw_windows <- function() {
  if (is.null(ps_env$boot_time)) {
    ps_env$boot_time <- .Call(ps__boot_time)
  }
  ps_env$boot_time
}
