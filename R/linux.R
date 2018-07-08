ps_pids_linux <- function() {
  sort(as.integer(dir("/proc", pattern = "^[0-9]+$")))
}

ps_pid_exists_linux <- function(pid) {
  if (! .Call(psp__pid_exists, as.integer(pid)))  {
    FALSE
  } else {
    ## Linux's apparently does not distinguish between PIDs and TIDs
    ## (thread IDs).
    ## listdir("/proc") won't show any TID (only PIDs) but
    ## os.stat("/proc/{tid}") will succeed if {tid} exists.
    ## os.kill() can also be passed a TID. This is quite confusing.
    ## In here we want to enforce this distinction and support PIDs
    ## only, see:
    ## https://github.com/giampaolo/psutil/issues/687
    tryCatch({
      ## Note: already checked that this is faster than using a
      ## regular expr. Also (a lot) faster than doing
      ## 'return pid in pids()'
      path <- sprintf("/proc/%i/status", pid)
      lines <- read_lines(path)
      tgidline <- grep("^Tgid:", lines, value = TRUE)[1]
      tgid <- strsplit(tgidline, "\\s+")[[1]][2]
      identical(as.integer(tgid), as.integer(pid))
    },
    error = function() {
        pid %in% ps_pids()
    })
  }
}

ps_boot_time_raw_linux <- function() {
  if (is.null(ps_env$boot_time)) {
    path <- sprintf("/proc/stat")
    btime <- grep("btime", read_lines(path), value = TRUE)
    ps_env$boot_time <-
      as.numeric(strsplit(str_strip(btime), "\\s+")[[1]][[2]])
  }
  ps_env$boot_time
}
