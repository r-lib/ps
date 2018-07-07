
ps_pids_windows <- function() {
  sort(.Call(psw__pids))
}

ps_pid_exists_windows <- function(pid) {
  .Call(psw__pid_exists, as.integer(pid))
}

ps_boot_time_raw_windows <- function() {
  if (is.null(ps_env$boot_time)) {
    ps_env$boot_time <- .Call(psw__boot_time)
  }
  ps_env$boot_time
}

ps_ppid_map_windows <- function() {
  pids <- .Call(psw__ppid_map)
  pidx <- seq(2L, length(pids), by = 2L)
  data.frame(
    pid = pids[pidx],
    ppid = pids[pidx-1L]
  )
}
