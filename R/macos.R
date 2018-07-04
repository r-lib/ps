
ps_pids_macos <- function() {
  ls <- .Call(psm__pids)
  ## 0 is missing from the list, usually, even though it is a process
  if (! 0L %in% ls && ps_pid_exists(0L)) {
    ls <- c(ls, 0L)
  }
  ls
}

ps_pid_exists_macos <- function(pid) {
  .Call(psp__pid_exists, as.integer(pid))
}

catch_zombie <- function(fun) {
  fun
  function(...) {
    tryCatch(
      fun(...),
      ps_error = function(err) {
        tryCatch(
          status <- self$status(),
          no_such_process = function() stop(err))
        if (status ==  "zombie") {
          stop(ps__zombie_process(self$.pid, self$.name, self$.ppid))
        } else {
          stop(ps__access_denied(self$.pid, self$.name))
        }
      }
    )
  }
}

#' @importFrom R6 R6Class

process_macos <- function() {
  if (is.null(ps_env$process_macos)) {
    ps_env$process_macos <- R6Class(
      "process_macos",
      cloneable = FALSE,
      inherit = process_posix(),
      public = list(

        exe = decorator(catch_zombie, function() {
          .Call(psm__proc_exe, self$.pid)
        }),

        cmdline = decorator(catch_zombie, function() {
          .Call(psm__proc_cmdline, self$.pid)
        }),

        environ = decorator(catch_zombie, function() {
          parse_envs(.Call(psm__proc_environ, self$.pid))
        }),

        ppid = function() {
          as.integer(self$.get_kinfo_proc()$ppid)
        },

        cwd = decorator(catch_zombie, function() {
          .Call(psm__proc_cwd, self$.pid)
        }),

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

        .create_time_raw = function() {
          self$.get_kinfo_proc()$ctime
        },

        num_threads = function() {
          self$.get_pidtaskinfo()$numthreads
        },

        status = function() {
          code <- self$.get_kinfo_proc()$status
          self$.proc_statuses()[code]
        },

        terminal = function() {
          num <- self$.get_kinfo_proc()$ttynr
          if (num == -1) return(NA_character_)
          tmap <- get_terminal_map()
          tmap[[as.character(num)]]
        },

        ## Internal methods
        .get_kinfo_proc = decorator(memoize_when_activated, function() {
          .Call(psm__proc_kinfo_oneshot, self$.pid)
        }),

        .get_pidtaskinfo = decorator(memoize_when_activated, catch_zombie,
                                     function() {
          .Call(psm__proc_pidtaskinfo_oneshot, self$.pid)
        }),

        .proc_name = function() {
          self$.get_kinfo_proc()$name
        },

        .proc_statuses = function() {
          x <- numeric()
          x[ps_env$constants$SIDL]   <- "idle"
          x[ps_env$constants$SRUN]   <- "running"
          x[ps_env$constants$SSLEEP] <- "sleeping"
          x[ps_env$constants$SSTOP]  <- "stopped"
          x[ps_env$constants$SZOMB]  <- "zombie"
          x
        },

        .oneshot_enter = function() {
          super$.oneshot_enter()
          self$.get_kinfo_proc$activate()
          self$.get_pidtaskinfo$activate()
        },

        .oneshot_exit = function() {
          super$.oneshot_exit()
          self$.get_kinfo_proc$deactivate()
          self$.get_pidtaskinfo$deactivate()
        }
      )
    )
  }

  ps_env$process_macos
}
