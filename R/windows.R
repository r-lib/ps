
ps_pids_windows <- function() {
  sort(.Call(psw__pids))
}

ps_pid_exists_windows <- function(pid) {
  .Call(psw__pid_exists, as.integer(pid))
}

ps_boot_time_raw_windows <- function() {
  if (is.null(ps_env$boot_time)) {
    ps_env$boot_time <- .Call(psw__boot_time)
  }
  ps_env$boot_time
}

#' @importFrom R6 R6Class

process_windows <- function() {
  if (is.null(ps_env$process_windows)) {
    ps_env$process_windows <- R6Class(
      "process_windows",
      cloneable = FALSE,
      inherit = process_common(),
      public = list(

        name = function() {
          ## This is how PIDs 0 and 4 are always represented in taskmgr
          ## and process-hacker.
          if (self$.pid == 0L) {
            "System Idle Process"
          } else if (self$.pid == 4L) {
            "System"
          }  else {
            tryCatch(
              ## Note: this will fail with AD for most PIDs owned
              ## by another user but it's faster.
              basename(self$get_exe()),
              error = function(e) {
                .Call(psw__proc_name, as.integer(self$.pid))
              }
            )
          }
        },

        exe = function() {
          ## Note: os.path.exists(path) may return False even if the file
          ## is there, see:
          ## http://stackoverflow.com/questions/3112546/os-path-exists-lies

          ## see https://github.com/giampaolo/psutil/issues/414
          ## see https://github.com/giampaolo/psutil/issues/528
          if (self$.pid == 0L || self$.pid == 4L) stop(ps__access_denied())
          convert_dos_path(.Call(psw__proc_exe, self$.pid))
        },

        cmdline = function() {
          .Call(psw__proc_cmdline, as.integer(self$.pid))
        },

        environ = function() {
          parse_envs(.Call(psw__proc_environ, self$.pid))
        },

        terminal = function() {
          NA_character_
        },

        ppid = decorator(memoize_when_activated, function() {
          if (!is.null(self$.ppid)) return(self$.ppid)
          map <- ps_ppid_map_windows()
          idx <- match(self$.pid, map[,1])
          if (is.na(idx)) stop(ps__no_such_process(self$.pid))
          self$.ppid <- map[idx, 2]
          self$.ppid
        }),

        cwd = function() {
          if (self$.pid == 0L || self$.pid == 4L) {
            stop(ps__access_denied())
          }
          sub("\\\\$", "", .Call(psw__proc_cwd, self$.pid))
        },

        username = function() {
          if (self$.pid == 0L || self$.pid == 4L) {
            "NT AUTHORITY\\SYSTEM"
          } else {
            domain_user <- .Call(psw__proc_username, self$.pid)
            paste0(domain_user[[1]], "\\", domain_user[[2]])
          }
        },

        memory_info = decorator(memoize_when_activated, function() {
          t <- self$.raw_meminfo()
          c(list(rss = t[["wset"]], vms = t[["pagefile"]]), t)
        }),

        cpu_times = decorator(memoize_when_activated, function() {
          ct <- tryCatch(
            c(.Call(psw__proc_cpu_times, self$.pid), NA_real_, NA_real_),
            error = function(e) {
              info <- self$.oneshot_info()
              c(info[["user_time"]], info[["kernel_time"]], NA_real_, NA_real_)
          })
          self$.common_pcputimes(ct)
        }),

        .create_time_raw = function() {
          if (self$.pid == 0L || self$.pid == 4L) {
            ps_boot_time_raw()
          } else {
            tryCatch(
              .Call(psw__proc_create_time, self$.pid),
              error = function(e) {
                self$.oneshot_info()[["create_time"]]
              }
            )
          }
        },

        num_threads = function() {
          self$.oneshot_info()[["num_threads"]]
        },

        suspend = decorator(assert_pid_not_reused, function() {
          .Call(psw__proc_suspend, self$.pid)
        }),

        resume = decorator(assert_pid_not_reused, function() {
          .Call(psw__proc_resume, self$.pid)
        }),

        kill = decorator(assert_pid_not_reused, function() {
          .Call(psw__proc_kill, self$.pid)
        }),

        status = function() {
          susp <- .Call(psw__proc_is_suspended, self$.pid)
          if (susp) "stopped" else "running"
        },

        .oneshot_info = decorator(memoize_when_activated, function() {
          .Call(psw__proc_info, self$.pid)
        }),

        .raw_meminfo = function() {
          tryCatch(
            .Call(psw__proc_memory_info, self$.pid),
            error = function(e) {
              info <- self$.oneshot_info()
              info[c("num_page_faults", "peak_wset", "wset", "peak_paged_pool",
                     "paged_pool", "peak_non_paged_pool", "non_paged_pool",
                     "pagefile", "peak_pagefile", "mem_private")]
            })
        },

        .oneshot_enter = function() {
          super$.oneshot_enter()
          self$.oneshot_info$activate()
        },

        .oneshot_exit = function() {
          super$.oneshot_exit()
          self$.oneshot_info$deactivate()
        },

        .ppid = NULL
      )
    )
  }

  ps_env$process_windows
}

#' @importFrom utils head

convert_dos_path <- function(path) {
  pcs <- strsplit(path, "\\", fixed =  TRUE)[[1]]
  rawdrive <- paste(head(pcs, 3), collapse = "\\")
  driveletter <- .Call(psw__win32_QueryDosDevice, rawdrive)
  paste0(driveletter, substr(path, nchar(rawdrive) + 1, nchar(path)))
}

ps_ppid_map_windows <- function() {
  pids <- .Call(psw__ppid_map)
  pidx <- seq(2L, length(pids), by = 2L)
  data.frame(
    pid = pids[pidx],
    ppid = pids[pidx-1L]
  )
}
