
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

#' @importFrom R6 R6Class

process_osx <- R6Class(
  "process_osx",
  public = list(
    initialize = function(pid)
      p_osx_init(self, private, pid),
    get_name = function()
      p_osx_get_name(self, private),
    get_exe = function()
      p_osx_get_exe(self, private),
    get_cmdline = function()
      p_osx_get_cmdline(self, private),
    get_environ = function(cached = TRUE)
      p_osx_get_environ(self, private, cached),
    get_ppid = function()
      p_osx_get_ppid(self, private),
    get_cwd = function()
      p_osx_get_cwd(self, private),
    get_uids = function()
      p_osx_get_uids(self, private),
    get_gids = function()
      p_osx_get_gids(self, private),
    get_memory_info = function()
      p_osx_get_memory_info(self, private),
    get_cpu_times = function()
      p_osx_get_cpu_times(self, private),
    get_create_time = function()
      p_osx_get_create_time(self, private),
    get_num_threads = function()
      p_osx_get_num_threads (self, private)

    ## TODO:
    ## - terminal
    ## - memory_full_info
    ## - num_ctx_switches
    ## - open_files
    ## - connections
    ## - num_fds
    ## - wait
    ## - nice_get
    ## - nice_set
    ## - status
    ## - threads
    ## - memory_maps
  ),

  private = list(
    pid = NULL,
    ppid = NULL,
    name = NULL,
    start = NULL,

    kinfo_proc = NULL,
    pidtaskinfo = NULL,
    environ = NULL,

    get_kinfo_proc = function(update = FALSE)
      p_osx__get_kinfo_proc(self, private, update),
    get_pidtaskinfo = function(update = FALSE)
      p_osx__get_pidtaskinfo(self, private, update)
  )
)

p_osx_init <- function(self, private, pid) {
  assert_that(is_pid(pid))
  private$pid <- as.integer(pid)
  invisible(self)
}

p_osx_get_name <- function(self, private) {
  private$get_kinfo_proc()$name
}

p_osx_get_exe <- function(self, private) {
  .Call(ps__proc_exe, private$pid)
}

p_osx_get_cmdline <- function(self, private) {
  .Call(ps__proc_cmdline, private$pid)
}

p_osx_get_environ <- function(self, private, cached) {
  if (is.null(private$environ) || !cached) {
    private$environ <- parse_envs(.Call(ps__proc_environ, private$pid))
  }
  private$environ
}

p_osx_get_ppid <- function(self, private) {
  private$get_kinfo_proc()$ppid
}

p_osx_get_cwd <- function(self, private) {
  .Call(ps__proc_cwd, private$pid)
}

p_osx_get_uids <- function(self, private) {
  kinfo <- private$get_kinfo_proc()
  common_puids(c(kinfo$ruid, kinfo$euid, kinfo$suid))
}

p_osx_get_gids <- function(self, private) {
  kinfo <- private$get_kinfo_proc()
  common_puids(c(kinfo$rgid, kinfo$egid, kinfo$sgid))
}

p_osx_get_memory_info <- function(self, private) {
  tinf <- private$get_pidtaskinfo(update = TRUE)
  tinf[c("rss", "vms", "pfaults", "pageins")]
}

p_osx_get_cpu_times <- function(self, private) {
  tinf <- private$get_pidtaskinfo(update = TRUE)
  common_pcputimes(c(tinf$cpuutime, tinf$cpustime, NA_real_, NA_real_))
}

p_osx_get_create_time <- function(self, private) {
  z <- private$get_kinfo_proc()$ctime
  as.POSIXct(z, origin = "1970-01-01", tz = "GMT")
}

p_osx_get_num_threads <- function(self, private) {
  private$get_pidtaskinfo(update = TRUE)$numthreads
}

## -----------------------------------------------------------------------
## Private mathods
## -----------------------------------------------------------------------

p_osx__get_kinfo_proc <- function(self, private, update) {
  if (update || is.null(private$kinfo_proc)) {
    private$kinfo_proc <- .Call(ps__proc_kinfo_oneshot, private$pid)
  }
  private$kinfo_proc
}

p_osx__get_pidtaskinfo <- function(self, private, update) {
  if (update || is.null(private$pidtaskinfo)) {
    private$pidtaskinfo <- .Call(ps__proc_pidtaskinfo_oneshot, private$pid)
  }
  private$pidtaskinfo
}
