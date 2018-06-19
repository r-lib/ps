
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
      p_osx_get_cmdline(self, private)
  ),

  private = list(
    pid = NULL,
    ppid = NULL,
    name = NULL,
    start = NULL,

    kinfo_proc = NULL,
    pidtaskinfo = NULL,

    get_kinfo_proc = function()
      p_osx__get_kinfo_proc(self, private),
    get_pidtaskinfo = function()
      p_osx__get_pidtaskinfo(self, private)
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

p_osx__get_kinfo_proc <- function(self, private) {
  if (is.null(private$kinfo_proc)) {
    private$kinfo_proc <- .Call(ps__proc_kinfo_oneshot, private$pid)
  }
  private$kinfo_proc
}

p_osx__get_pidtaskinfo <- function(self, private) {
  if (is.null(private$pidtaskinfo)) {
    private$pidtaskinfo <- .Call(ps__proc_pidtaskinfo_oneshot, private$pid)
  }
  private$pidtaskinfo
}
