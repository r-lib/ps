
#' Ids of all processes on the system
#'
#' @return Integer vector of process ids.
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  if (os[["OSX"]])
    ps_pids_osx()
  else if (os[["LINUX"]])
    ps_pids_linux()
  else if (os[["WINDOWS"]])
    ps_pids_windows()
  else
    stop("Not implemented for this platform")
}

#' Check if a process id exists
#'
#' This is faster than calling [ps_pids()] first.
#'
#' @param pid Process id, integer scalar.
#' @return Logical scalar.
#'
#' @export

ps_pid_exists <- function(pid) {
  assert_that(is_pid(pid))
  os <- ps_os_type()
  if (os[["OSX"]])
    ps_pid_exists_osx(pid)
  else if (os[["LINUX"]])
    ps_pid_exists_linux(pid)
  else if (os[["WINDOWS"]])
    ps_pid_exists_windows(pid)
  else
    stop("Not implemented for this platform")
}

#' Boot time of the system
#'
#' @return A `POSIXct` object.
#'
#' @export

ps_boot_time <- function() {
  os <- ps_os_type()
  if (os[["LINUX"]])
    ps_boot_time_linux()
  else if (os[["WINDOWS"]])
    ps_boot_time_windows()
  else
    stop("Not implemented for this platform")
}

#' Process table
#'
#' @param user Username, to filter the results to matching processes.
#' @param after Start time (POSIXt), to filter the results to processes
#'   that started after this.
#' @return Data frame (tibble), see columns below.
#'
#' Columns:
#' * `pid`: Process ID.
#' * `ppid`: Process ID of parent process.
#' * `name`: Process name.
#' * `username`: Name of the user (real uid on POSIX).
#' * `status`: I.e. *running*, *sleeping*, etc.
#' * `user`: User CPU time.
#' * `system`: System CPU time.
#' * `rss`: Resident set size, the amount of memory the process currently
#'    uses. Does not include memory that is swapped out. It does include
#'    shared libraries.
#' * `vms`: Virtual memory size. All memory the process has access to.
#' * `created`: Time stamp when the process was created.
#'
#' @importFrom prettyunits pretty_bytes
#' @export

ps <- function(user = NULL, after = NULL) {
  assert_that(is.null(user) || is_string(user))
  assert_that(is.null(after) || inherits(after, "POSIXt"))

  pids <- ps_pids()
  processes <- not_null(lapply(pids, function(p) {
    tryCatch(process(p), no_such_process = function(e) NULL) }))

  ct <- NULL
  if (!is.null(after)) {
    ct <- lapply(processes, function(p) p$create_time())
    selected <- ct >= after
    processes <- processes[selected]
    ct <- ct[selected]
  }

  us <- NULL
  if (!is.null(user)) {
    us <- map_chr(processes, function(p) p$username())
    selected <- us == user
    processes <- processes[selected]
    us <- us[selected]
  }

  us <- us %||% map_chr(processes, function(p)
    fallback(p$username(), NA_character_))
  ct <- ct %||% lapply(processes, function(p)
    fallback(p$create_time(), NA_time()))
  pd <- map_int(processes, function(p) fallback(p$pid(), NA_integer_))
  pp <- map_int(processes, function(p) fallback(p$ppid(), NA_integer_))
  nm <- map_chr(processes, function(p) fallback(p$name(), NA_character_))
  st <- map_chr(processes, function(p) fallback(p$status(), NA_character_))
  time <- lapply(processes, function(p) fallback(p$cpu_times(), NULL))
  cpt <- map_dbl(time, function(x) x[["user"]] %||% NA_real_)
  cps <- map_dbl(time, function(x) x[["system"]] %||% NA_real_)
  mem <- lapply(processes, function(p) fallback(p$memory_info(), NULL))
  rss <- map_chr(mem, function(x) pretty_bytes_na(x[["rss"]] %||% NA_real_))
  vms <- map_chr(mem, function(x) pretty_bytes_na(x[["vms"]] %||% NA_real_))

  pss <- data.frame(
    stringsAsFactors = FALSE,
    pid = pd,
    ppid = pp,
    name = nm,
    username = us,
    status = st,
    user = cpt,
    system = cps,
    rss = rss,
    vms = vms,
    created = format_unix_time(unlist(ct))
  )

  pss <- pss[order(-as.numeric(pss$created)), ]

  class(pss) <- unique(c("tbl_df", "tbl", class(pss)))
  pss
}

#' Parent proccesses for each process
#'
#' On Windows systems the parent process might not exist any more, and
#' its pid might have been reused, potentially.
#'
#' On POSIX systems, if the parent process exited already, usually PID 1
#' is shown instead of its id.
#'
#' @return A two-column data frame with integer columns `pid` and `ppid`.
#'
#' @export

ps_ppid_map <- function() {
  os <- ps_os_type()
  if (os[["WINDOWS"]]) {
    ps_ppid_map_windows()
  } else {
    pids <- ps_pids()
    processes <- not_null(lapply(pids, function(p) {
      tryCatch(process(p), no_such_process = function(e) NULL) }))
    pd <- map_int(processes, function(p) fallback(p$pid(), NA_integer_))
    pp <- map_int(processes, function(p) fallback(p$ppid(), NA_integer_))
    data.frame(
      pid = pd,
      ppid = pp
    )
  }
}
