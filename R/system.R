
#' Ids of all processes on the system
#'
#' @return Integer vector of process ids.
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  pp <- if (os[["MACOS"]])
    ps_pids_macos()
  else if (os[["LINUX"]])
    ps_pids_linux()
  else if (os[["WINDOWS"]])
    ps_pids_windows()
  else
    stop("Not implemented for this platform")

  sort(pp)
}

ps_pids_windows <- function() {
  sort(.Call(ps__pids))
}

ps_pids_macos <- function() {
  ls <- .Call(ps__pids)
  ## 0 is missing from the list, usually, even though it is a process
  if (! 0L %in% ls && ps_pid_exists_macos(0L)) {
    ls <- c(ls, 0L)
  }
  ls
}

ps_pid_exists_macos <- function(pid) {
  .Call(psp__pid_exists, as.integer(pid))
}

ps_pids_linux <- function() {
  sort(as.integer(dir("/proc", pattern = "^[0-9]+$")))
}

#' Boot time of the system
#'
#' @return A `POSIXct` object.
#'
#' @export

ps_boot_time <- function() {
  format_unix_time(.Call(ps__boot_time))
}

#' Number of logical or physical CPUs
#'
#' @param logical If `FALSE` the number of phyical cores only. If cannot
#' be determined, it returns `NA`.
#' @return Integer scalar, the number of CPUs.
#'
#' @export

ps_cpu_count <- function(logical = TRUE) {
  assert_flag(logical)
  if (logical) ps_cpu_count_logical() else ps_cpu_count_physical()
}

ps_cpu_count_logical <- function() {
  .Call(ps__cpu_count_logical)
}

ps_cpu_count_physical <- function() {
  .Call(ps__cpu_count_physical)
}
