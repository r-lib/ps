
#' @useDynLib ps, .registration = TRUE
NULL

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
    tryCatch(ps_handle(p), no_such_process = function(e) NULL) }))

  ct <- NULL
  if (!is.null(after)) {
    ct <- lapply(processes, ps_create_time)
    selected <- ct >= after
    processes <- processes[selected]
    ct <- ct[selected]
  }

  us <- NULL
  if (!is.null(user)) {
    us <- map_chr(processes, function(p) ps_username(p))
    selected <- us == user
    processes <- processes[selected]
    us <- us[selected]
  }

  us <- us %||% map_chr(processes, function(p)
    fallback(ps_username(p), NA_character_))
  ct <- ct %||% lapply(processes, function(p)
    fallback(ps_create_time(p), NA_time()))
  pd <- map_int(processes, function(p) fallback(ps_pid(p), NA_integer_))
  pp <- map_int(processes, function(p) fallback(ps_ppid(p), NA_integer_))
  nm <- map_chr(processes, function(p) fallback(ps_name(p), NA_character_))
  st <- map_chr(processes, function(p) fallback(ps_status(p), NA_character_))
  time <- lapply(processes, function(p) fallback(ps_cpu_times(p), NULL))
  cpt <- map_dbl(time, function(x) x[["user"]] %||% NA_real_)
  cps <- map_dbl(time, function(x) x[["system"]] %||% NA_real_)
  mem <- lapply(processes, function(p) fallback(ps_memory_info(p), NULL))
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
