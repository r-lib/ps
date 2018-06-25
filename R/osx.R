
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

process_osx <- function() {
  if (is.null(ps_env$process_osx)) {
    ps_env$process_osx <- R6Class(
      "process_osx",
      cloneable = FALSE,
      inherit = process_posix(),
      public = list(

        name = function() {
          self$.get_kinfo_proc()$name
        },

        exe = function() {
          .Call(ps__proc_exe, self$.pid)
        },

        cmdline = function() {
          .Call(ps__proc_cmdline, self$.pid)
        },

        environ = function() {
          parse_envs(.Call(ps__proc_environ, self$.pid))
        },

        ppid = function() {
          as.integer(self$.get_kinfo_proc()$ppid)
        },

        cwd = function() {
          .Call(ps__proc_cwd, self$.pid)
        },

        uids = function() {
          kinfo <- self$.get_kinfo_proc()
          self$.common_puids(c(kinfo$ruid, kinfo$euid, kinfo$suid))
        },

        gids = function() {
          kinfo <- self$.get_kinfo_proc()
          self$.common_puids(c(kinfo$rgid, kinfo$egid, kinfo$sgid))
        },

        memory_info = function() {
          tinf <- self$.get_pidtaskinfo()
          tinf[c("rss", "vms", "pfaults", "pageins")]
        },

        cpu_times = function() {
          tinf <- self$.get_pidtaskinfo()
          self$.common_pcputimes(
                 c(tinf$cpuutime, tinf$cpustime, NA_real_, NA_real_))
        },

        create_time = function() {
          z <- self$.get_kinfo_proc()$ctime
          self$.format_unix_time(z)
        },

        num_threads = function() {
          self$.get_pidtaskinfo()$numthreads
        },

        status = function() {
          code <- self$.get_kinfo_proc()$status
          self$.proc_statuses()[code]
        },

        ## Internal methods
        .get_kinfo_proc = function() {
          .Call(ps__proc_kinfo_oneshot, self$.pid)
        },

        .get_pidtaskinfo = function() {
          .Call(ps__proc_pidtaskinfo_oneshot, self$.pid)
        },

        .proc_statuses = function() {
          x <- numeric()
          x[ps_env$constants$SIDL]   <- "idle"
          x[ps_env$constants$SRUN]   <- "running"
          x[ps_env$constants$SSLEEP] <- "sleeping"
          x[ps_env$constants$SSTOP]  <- "stopped"
          x[ps_env$constants$SZOMB]  <- "zombie"
          x
        }
      )
    )
  }

  ps_env$process_osx
}
