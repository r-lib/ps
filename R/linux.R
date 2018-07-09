ps_pids_linux <- function() {
  sort(as.integer(dir("/proc", pattern = "^[0-9]+$")))
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
