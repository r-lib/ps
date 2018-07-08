
#' Create a system process object
#'
#' Create an R6 object that represents a system process.
#'
#' @section Methods:
#'
#' * `p$pid()`  (1)
#'
#'   Returns the process id.
#'
#' * `p$create_time()` (1)
#'
#'   Time stamp for the process creation, according to the OS. ps uses this
#'   as an id, together with the pid.
#'
#' * `p$is_running()` (1)
#'
#'   Checks if the process is still running, returns `TRUE` or `FALSE`.
#'   It returns the correct answer, even if the process has finished and
#'   its pid was reused.
#'
#' * `p$status()` (2)
#'
#'   One of the following:
#'   * `"idle"`: Process being created by fork, macOS only.
#'   * `"running"`: Currently runnable on macOS and Windows. Actually
#'       running on Linux.
#'   * `"sleeping"` Sleeping on a wait or poll.
#'   * `"disk_sleep"` Uninterruptible sleep, waiting for an I/O operation
#'      (Linux only).
#'   * `"stopped"` Stopped, either by a job control signal or because it
#'      is being traced.
#'   * `"tracing_stop"` Stopped for tracing (Linux only).
#'   * `"zombie"` Zombie. Finished, but parent has not read out the exit
#'      status yet.
#'   * `"dead"` Should never be seen (Linux).
#'   * `"wake_kill"` Received fatal signal (Linux only).
#'   * `"waking"` Paging (Linux only, not valid since the 2.6.xx kernel).
#'
#' * `p$name()` (2)
#'
#'   Name of the process, typically the name of the executable. Note that
#'   on Unix, this can change, e.g. via an `exec*()` system call.
#'
#' * `p$exe()` (2)(4)
#'
#'   Path to the executable of the process. May also be an empty string or
#'   `NA` if it cannot be determined.
#'
#' * `p$cmdline()` (2)(4)
#'
#'   Command line of the process, i.e. the executable and the command line
#'   arguments, in a character vector. On Unix the program might change its
#'   command line, and some programs actually do it.
#'
#' * `p$cwd()` (2)(4)
#'
#'   Process current working directory as an absolute path.
#'
#' * `p$environ()` (2)(4)
#'
#'   The environment variables of the process, in a named vector, similarly
#'   to the return value of `Sys.getenv()` (without arguments). Note: this
#'   usually does not reflect changes made after the process started.
#'
#' * `p$environ_raw()` (2)(4)
#'
#'   Similar to `p$environ()` but returns the unparsed `"var=value"`
#'   strings.
#'
#' * `p$username()` (2)
#'
#'   The name of the user that owns the process. On Unix it is calculated
#'   from the real user id.
#'
#'   On Unix, a numeric uid id returned if the uid is not in the user
#'   database, thus a username cannot be determined.
#'
#' * `p$uids()` (2)
#' * `p$guids()` (2)
#'
#'   User ids and group ids of the process. Not implemented on Windows,
#'   throws `not_implemented` error. Both return integer vectors with names:
#'   `real`, `effective` and `saved`.
#'
#' * `p$ppid()` (2)
#'
#'   Returns the parent process's id. Note that the ppid of a process might
#'   change on Unix: typically when the parent process quits, it is set to 1,
#'   which is the init process. (But not always, Linux can be configured to
#'   use another pid, and pid 1 is usually not init in Docker.)
#'
#'   On Windows the ppid is not updated when the parent process exits, so
#'   it might refer to a non-existant process, or even a different process
#'   if the parent pid was reused.
#'
#'   The `parent()` method works around the Windows issue, by comparing
#'   process creation times.
#'
#' * `p$parent()` (2)
#'
#'   Returns a process object for the parent process. On Unix, it will
#'   return the reassigned (typically pid 1) process, if the real parent
#'   process has quit aleady. On Windows it fails with `no_such_process`
#'    error in this case.
#'
#' * `p$children(recursive = FALSE)` (TODO)
#'
#'   List of child processes (process objects) of the process. Note that
#'   this typically requires enumerating all processes on the system, so
#'   it is a costly operation.
#'
#'   Arguments:
#'
#'   * `recursive`: whether to include the children of the children, etc.
#'
#' * `p$terminal()` (2)
#'
#'   Returns the terminal of the process. Not implemented on Windows, always
#'   returns `NA_character_`. On Unix it returns `NA_character_` if the
#'   process has no terminal.
#'
#' * `p$num_threads()` (2)(4)
#'
#'   The number threads.
#'
#' * `p$cpu_times()` (2)(4)
#'
#'   Retuns a named real vector: `user`, `system`, `children_user`,
#'   `children_system`, all in seconds. The children times are typically
#'   only available on Linux, and are `NA` on other platforms.
#'
#'   Explanations for the numbers, from the Linux stat(5) manual page:
#'   * `user`: Amount of time that this process has been scheduled in user
#'     mode.
#'   * `system`: Amount of time that this process has been scheduled in
#'     kernel mode
#'   * `childen_user`: On Linux, amount of time that this process's
#'     waited-for children have been scheduled in user mode.
#'   * `children_system`: On Linux, Amount of time that this process's
#'     waited-for children have been scheduled in kernel mode.
#'
#' * `p$memory_imfo()` (2)(4)
#'
#'   A list with information about memory usage. Portable fields:
#'   * `rss`: "Resident Set Size", this is the non-swapped physical memory a
#'     process has used. On UNIX it matches "top"‘s RES column (see doc). On
#'     Windows this is an alias for `wset` field and it matches "Memory"
#'     column of `taskmgr.exe`.
#'   * `vmem`: "Virtual Memory Size", this is the total amount of virtual
#'     memory used by the process. On UNIX it matches "top"‘s VIRT column
#'     (see doc). On Windows this is an alias for the `pagefile` field and
#'     it matches the "Working set (memory)" column of `taskmgr.exe`.
#'
#'   Non-portable fields:
#'   * `shared`: (Linux) memory that could be potentially shared with other
#'     processes. This matches "top"‘s SHR column (see doc).
#'   * `text`: (Linux): aka TRS (text resident set) the amount of memory
#'     devoted to executable code. This matches "top"‘s CODE column (see
#'     doc).
#'   * `data`: (Linux): aka DRS (data resident set) the amount of physical
#'     memory devoted to other than executable code. It matches "top"‘s
#'     DATA column (see doc).
#'   * `lib`: (Linux): the memory used by shared libraries.
#'   * `dirty`: (Linux): the number of dirty pages.
#'   * `pfaults`: (macOS): number of page faults.
#'   * `pageins`: (macOS): number of actual pageins.
#'
#'   For on explanation of Windows fields see the
#'   [PROCESS_MEMORY_COUNTERS_EX](http://msdn.microsoft.com/en-us/library/windows/desktop/ms684874(v=vs.85).aspx)
#'   structure.
#'
#' * `p$send_signal(sig)` (3)
#'
#'   Send a signal to the process. Not implemented on Windows. See
#'   [signals()] for the list of signals on the current platform.
#'
#'   Arguments:
#'
#'   * `sig`: The signal number, see [signals()].
#'
#' * `p$suspend()` (3)
#'
#'   Suspend process execution with `SIGSTOP` pre-emptively checking
#'   whether PID has been reused. On Windows this has the effect of
#'   suspending all process threads.
#'
#' * `p$resume()` (3)
#'
#'   Resume process execution with SIGCONT pre-emptively checking
#'   whether PID has been reused. On Windows this has the effect of resuming
#'   all process threads.
#'
#' * `p$terminate()` (3)
#'
#'   Send a `SIGTERM` signal to the process. Not implemented on Windows.
#'
#' * `p$kill()` (3)
#'
#'   Kill the current process with SIGKILL pre-emptively checking
#'   whether PID has been reused.
#'
#' @section Notes:
#'
#' (1) This method works, even if the process has already terminated.
#'     Other methods throw a `"no_such_process"` error.
#'
#' (2) This method fails if the process has already terminated, with
#'     a `"no_such_process"` error.
#'
#' (3) This method checks if the process is still running, before
#'     performing its action, and throws a `"no_such_process"` error if
#'     the process is not available any more.
#'
#' (4) Throws a `"zombie_process"` error for zombie processes.
#'
#' @name process
#' @importFrom R6 R6Class
#' @export
NULL

process <- R6Class(
  "process",

  public = list(

    initialize = function(pid = NULL, time = NULL) {
      private$handle <- ps_handle(pid, time)
      invisible(self)
    },

    format = function(...) format(private$handle),

    print = function(...) {
      print(private$handle)
      invisible(self)
    },

    pid = function() ps_pid(private$handle),

    create_time = function() ps_create_time(private$handle),

    is_running = function() ps_is_running(private$handle),

    parent = function() ps_parent(private$handle),

    ppid = function() ps_ppid(private$handle),

    name = function() ps_name(private$handle),

    exe = function() ps_exe(private$handle),

    cmdline = function() ps_cmdline(private$handle),

    status = function() ps_status(private$handle),

    username = function() ps_username(private$handle),

    cwd = function() ps_cwd(private$handle),

    uids = function() ps_uids(private$handle),

    gids = function() ps_gids(private$handle),

    terminal = function() ps_terminal(private$handle),

    environ = function() ps_environ(private$handle),

    environ_raw = function() ps_environ_raw(private$handle),

    num_threads = function() ps_num_threads(private$handle),

    cpu_times = function() ps_cpu_times(private$handle),

    memory_info = function() ps_memory_info(private_handle),

    send_signal = function(sig) ps_send_signal(private_handle, sig),

    suspend = function() ps_suspend(private$handle),

    resume = function() ps_resume(private$handle),

    terminate = function() ps_terminate(private$handle),

    kill = function() ps_kill(private$handle)
  ),

  private = list(
    handle = NULL
  )
)
