
#' Create a process handle
#'
#' @param pid Process id. Integer scalar. `NULL` means the current R
#'   process.
#' @param time Start time of the process. Usually `NULL` and ps will query
#'   the start time.
#' @return `ps_handle()` returns a process handle (class `ps_handle`).
#'
#' @family process handle functions
#' @export

ps_handle <- function(pid = NULL, time = NULL) {
  .Call(psll_handle, pid, time)
}

#' @param x Process handle.
#' @param ... Not used currently.
#'
#' @rdname ps_handle
#' @export

format.ps_handle <- function(x, ...) {
  pieces <- .Call(psll_format, x)
  paste0("<ps::ps_handle> PID=", pieces[[2]], ", NAME=", pieces[[1]],
         ", AT=", format_unix_time(pieces[[3]]))
}

#' @rdname ps_handle
#' @export

print.ps_handle <- function(x, ...)  {
  cat(format(x, ...),  "\n", sep = "")
  invisible(x)
}

#' Pid of a process handle
#'
#' This function works even if the process has already finished.
#'
#' @param p Process handle.
#' @return Process id.
#'
#' @family process handle functions
#' @export

ps_pid <- function(p) {
  .Call(psll_pid, p)
}

#' Start time of a process
#'
#' The pid and the start time pair serves as the identifier of the process,
#' as process ids might be reused, but the chance of starting two processes
#' with identical ids within the resolution of the timer is minimal.
#'
#' This function works even if the process has already finished.
#'
#' @param p Process handle.
#' @return `POSIXct` object, start time, in GMT.
#'
#' @family process handle functions
#' @export

ps_create_time <- function(p) {
  format_unix_time(.Call(psll_create_time, p))
}

#' Checks whether a process is running
#'
#' It returns `FALSE` if the process has already finished.
#'
#' It uses the start time of the process to work around pid reuse. I.e.
#  it returns the correct answer, even if the process has finished and
#  its pid was reused.
#'
#' @param p Process handle.
#' @return Logical scalar.
#'
#' @family process handle functions
#' @export

ps_is_running <- function(p) {
  .Call(psll_is_running, p)
}

#' Parent pid or parent process of a process
#'
#' `ps_ppid()` returns the parent pid, `ps_parent()` returns a `ps_handle`
#' of the parent.
#'
#' On POSIX systems, if the parent process terminates, another process
#' (typically the pid 1 process) is marked as parent. `ps_ppid()` and
#' `ps_parent()` will return this process then.
#'
#' Both `ps_ppid()` and `ps_parent()` work for zombie processes.
#'
#' @param p Process handle.
#' @return `ps_ppid()` returns and integer scalar, the pid of the parent
#'   of `p`. `ps_parent()` returns a `ps_handle`.
#'
#' @family process handle functions
#' @export

ps_ppid <- function(p) {
  .Call(psll_ppid, p)
}

#' @rdname  ps_ppid
#' @family process handle functions
#' @export

ps_parent <- function(p) {
  .Call(psll_parent, p)
}

#' Process name
#'
#' The name of the program, which is typically the name of the executable.
#'
#' On on Unix this can change, e.g. via an exec*() system call.
#'
#' `ps_name()` works on zombie processes.
#'
#' @param p Process handle.
#' @return Character scalar.
#'
#' @family process handle functions
#' @export

ps_name <- function(p) {
  n <- .Call(psll_name, p)
  if (nchar(n) >= 15) {
    ## On UNIX the name gets truncated to the first 15 characters.
    ## If it matches the first part of the cmdline we return that
    ## one instead because it's usually more explicative.
    ## Examples are "gnome-keyring-d" vs. "gnome-keyring-daemon".
    cmdline <- tryCatch(
      ps_cmdline(p),
      access_denied = function(e) NULL
    )
    if (!is.null(cmdline)) {
      exname <- basename(cmdline[1])
      if (str_starts_with(exname, n)) n <- exname
    }
  }
  n
}

#' Full path of the executable of a process
#'
#' Path to the executable of the process. May also be an empty string or
#' `NA` if it cannot be determined.
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return Character scalar.
#'
#' @family process handle functions
#' @export

ps_exe <- function(p) {
  .Call(psll_exe, p)
}

#' Command line of the process
#'
#' Command line of the process, i.e. the executable and the command line
#' arguments, in a character vector. On Unix the program might change its
#' command line, and some programs actually do it.
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return Character vector.
#'
#' @family process handle functions
#' @export

ps_cmdline <- function(p) {
  .Call(psll_cmdline, p)
}

#' Current process status
#'
#' One of the following:
#' * `"idle"`: Process being created by fork, macOS only.
#' * `"running"`: Currently runnable on macOS and Windows. Actually
#'     running on Linux.
#' * `"sleeping"` Sleeping on a wait or poll.
#' * `"disk_sleep"` Uninterruptible sleep, waiting for an I/O operation
#'    (Linux only).
#' * `"stopped"` Stopped, either by a job control signal or because it
#'    is being traced.
#' * `"tracing_stop"` Stopped for tracing (Linux only).
#' * `"zombie"` Zombie. Finished, but parent has not read out the exit
#'    status yet.
#' * `"dead"` Should never be seen (Linux).
#' * `"wake_kill"` Received fatal signal (Linux only).
#' * `"waking"` Paging (Linux only, not valid since the 2.6.xx kernel).
#'
#' Works for zombie processes.
#'
#' @param p Process handle.
#' @return Character scalar.
#'
#' @family process handle functions
#' @export

ps_status <- function(p) {
  .Call(psll_status, p)
}


#' Owner of the process
#'
#' The name of the user that owns the process. On Unix it is calculated
#' from the real user id.
#'
#' On Unix, a numeric uid id returned if the uid is not in the user
#' database, thus a username cannot be determined.
#'
#' Works for zombie processes.
#'
#' @param p Process handle.
#' @return String scalar.
#'
#' @family process handle functions
#' @export

ps_username <- function(p) {
  .Call(psll_username, p)
}

#' Process current working directory as an absolute path.
#'
#' For a zombie process it throws a `zombie_process` error.
#'
#' @param p Process handle.
#' @return String scalar.
#'
#' @family process handle functions
#' @export

ps_cwd <- function(p) {
  .Call(psll_cwd, p)
}

#' User ids and group ids of the process
#'
#' User ids and group ids of the process. Both return integer vectors with
#' names: `real`, `effective` and `saved`.
#'
#' Both work for zombie processes.
#'
#' They are not implemented on Windows, they throw a `not_implemented`
#' error.
#'
#' @param p Process handle.
#' @return Named integer vector of length 3, with names: `real`,
#'   `effective` and `saved`.
#'
#' @seealso [ps_username()] returns a user _name_ and works on all
#'   platforms.
#' @family process handle functions
#' @export

ps_uids <- function(p) {
  .Call(psll_uids, p)
}

#' @rdname ps_uids
#' @family process handle functions
#' @export

ps_gids <- function(p) {
  .Call(psll_gids, p)
}

#' Terminal device of the process
#'
#' Returns the terminal of the process. Not implemented on Windows, always
#' returns `NA_character_`. On Unix it returns `NA_character_` if the
#' process has no terminal.
#'
#' Works for zombie processes.
#'
#' @param p Process handle.
#' @return Character scalar.
#'
#' @family process handle functions
#' @export

ps_terminal <- function(p) {
  ttynr <- .Call(psll_terminal, p)
  if (is.na(ttynr)) {
    NA_character_
  } else {
    tmap <- get_terminal_map()
    tmap[[as.character(ttynr)]]
  }
}

#' Environment variables of a process
#'
#' `ps_environ()` returns the environment variables of the process, in a
#' named vector, similarly to the return value of `Sys.getenv()`
#' (without arguments).
#'
#' Note: this usually does not reflect changes made after the process
#' started.
#'
#' `ps_environ_raw()` is similar to `p$environ()` but returns the
#' unparsed `"var=value"` strings. This is faster, and sometimes good
#' enough.
#'
#' These functions throw a `zombie_process` error for zombie processes.
#'
#' @param p Process handle.
#' @return `ps_environ()` returns a named character vector (that has a
#' `Dlist` class, so it is printed nicely), `ps_environ_raw()` returns a
#' character vector.
#'
#' @family process handle functions
#' @export

ps_environ <- function(p) {
  parse_envs(.Call(psll_environ, p))
}

#' @rdname ps_environ
#' @family process handle functions
#' @export

ps_environ_raw <- function(p) {
  .Call(psll_environ, p)
}

#' Number of threads
#'
#' Throws a `zombie_process()` error for zombie processes.
#'
#' @param p Process handle.
#' @return Integer scalar.
#'
#' @family process handle functions
#' @export

ps_num_threads <- function(p) {
  .Call(psll_num_threads, p)
}

#' CPU times of the process
#'
#' All times are measued in seconds:
#' * `user`: Amount of time that this process has been scheduled in user
#'   mode.
#' * `system`: Amount of time that this process has been scheduled in
#'   kernel mode
#' * `childen_user`: On Linux, amount of time that this process's
#'   waited-for children have been scheduled in user mode.
#' * `children_system`: On Linux, Amount of time that this process's
#'   waited-for children have been scheduled in kernel mode.
#'
#' Throws a `zombie_process()` error for zombie processes.
#'
#' @param p Process handle.
#' @return Named real vector or length four: `user`, `system`,
#'   `childen_user`,  `children_system`. The last two are `NA` on
#'   non-Linux systems.
#'
#' @family process handle functions
#' @export

ps_cpu_times <- function(p) {
  .Call(psll_cpu_times, p)
}

#' Throws a `zombie_process()` error for zombie processes.
#'
#' A list with information about memory usage. Portable fields:
#' * `rss`: "Resident Set Size", this is the non-swapped physical memory a
#'   process has used. On UNIX it matches "top"‘s RES column (see doc). On
#'   Windows this is an alias for `wset` field and it matches "Memory"
#'   column of `taskmgr.exe`.
#' * `vmem`: "Virtual Memory Size", this is the total amount of virtual
#'   memory used by the process. On UNIX it matches "top"‘s VIRT column
#'   (see doc). On Windows this is an alias for the `pagefile` field and
#'   it matches the "Working set (memory)" column of `taskmgr.exe`.
#'
#' Non-portable fields:
#' * `shared`: (Linux) memory that could be potentially shared with other
#'   processes. This matches "top"‘s SHR column (see doc).
#' * `text`: (Linux): aka TRS (text resident set) the amount of memory
#'   devoted to executable code. This matches "top"‘s CODE column (see
#'   doc).
#' * `data`: (Linux): aka DRS (data resident set) the amount of physical
#'   memory devoted to other than executable code. It matches "top"‘s
#'   DATA column (see doc).
#' * `lib`: (Linux): the memory used by shared libraries.
#' * `dirty`: (Linux): the number of dirty pages.
#' * `pfaults`: (macOS): number of page faults.
#' * `pageins`: (macOS): number of actual pageins.
#'
#' For on explanation of Windows fields see the
#' [PROCESS_MEMORY_COUNTERS_EX](http://msdn.microsoft.com/en-us/library/windows/desktop/ms684874(v=vs.85).aspx)
#' structure.
#'
#' @param p Process handle.
#' @return Named real vector.
#'
#' @family process handle functions
#' @export

ps_memory_info <- function(p) {
  .Call(psll_memory_info, p)
}

#' Send signal to a process
#'
#' Send a signal to the process. Not implemented on Windows. See
#' [signals()] for the list of signals on the current platform.
#'
#' It checks if the process is still running, before sending the signal,
#' to avoid signalling the wrong process, because of pid reuse.
#'
#' @param p Process handle.
#' @param sig Signal number, see [signals()].
#'
#' @family process handle functions
#' @export

ps_send_signal <- function(p, sig) {
  .Call(psll_send_signal, p, sig)
}

#' Suspend (stop) the process
#'
#' Suspend process execution with `SIGSTOP` pre-emptively checking
#' whether PID has been reused. On Windows this has the effect of
#' suspending all process threads.
#'
#' @param p Process handle.
#'
#' @family process handle functions
#' @export

ps_suspend <- function(p) {
  .Call(psll_suspend, p)
}

#' Resume (continue) a stopped process
#'
#' Resume process execution with SIGCONT pre-emptively checking
#' whether PID has been reused. On Windows this has the effect of resuming
#' all process threads.
#'
#' @param p Process handle.
#'
#' @family process handle functions
#' @export

ps_resume <- function(p) {
  .Call(psll_resume, p)
}

#' Terminate a Unix process
#'
#' Send a `SIGTERM` signal to the process. Not implemented on Windows.
#'
#' Checks if the process is still running, to work around pid reuse.
#'
#' @param p Process handle.
#'
#' @family process handle functions
#' @export

ps_terminate <- function(p) {
  .Call(psll_terminate, p)
}

#' Kill a process
#'
#' Kill the current process with SIGKILL pre-emptively checking
#' whether PID has been reused. On Windows it uses `TerminateProcess()`.
#'
#' @param p Process handle.
#'
#' @family process handle functions
#' @export

ps_kill <- function(p) {
  .Call(psll_kill, p)
}
