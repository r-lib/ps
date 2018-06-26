
ps_pids_windows <- function() {
  sort(.Call(ps__pids))
}

ps_pid_exists_windows <- function(pid) {
  .Call(ps__pid_exists, as.integer(pid))
}

ps_boot_time_windows <- function() {
  if (is.null(ps_env$boot_time)) {
    bt <- .Call(ps__boot_time)
    ps_env$boot_time <- as.POSIXct(bt, origin = "1970-01-01", tz = "GMT")
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
                .Call(ps__proc_name, as.integer(self$.pid))
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
          convert_dos_path(.Call(ps__proc_exe, self$.pid))
        },

        cmdline = function() {
          .Call(ps__proc_cmdline, as.integer(self$.pid))
        },

        environ = function(cached = TRUE) {
          parse_envs(.Call(ps__proc_environ, self$.pid))
        },

        ppid = function() {
          map <- ps_ppid_map_windows()
          idx <- match(self$.pid, map[,1])
          if (is.na(idx)) stop(ps__no_such_process(self$.pid))
          map[idx, 2]
        },

        cwd = function() {
          if (self$.pid == 0L || self$.pid == 4L) {
            stop(ps__access_denied())
          }
          sub("\\\\$", "", .Call(ps__proc_cwd, self$.pid))
        },

        username = function() {
          if (self$.pid == 0L || self$.pid == 4L) {
            "NT AUTHORITY\\SYSTEM"
          } else {
            domain_user <- .Call(ps__proc_username, self$.pid)
            paste0(domain_user[[1]], "\\", domain_user[[2]])
          }
        },

        memory_info = function() {
          t <- self$.raw_meminfo()
          c(list(rss = t[["wset"]], vms = t[["pagefile"]]), t)
        },

        cpu_times = function() {
          ct <- tryCatch(
            c(.Call(ps__proc_cpu_times, self$.pid), NA_real_, NA_real_),
            error = function(e) {
              info <- self$.oneshot_info(cached = FALSE)
              c(info[["user_time"]], info[["kernel_time"]], NA_real_, NA_real_)
          })
          self$.common_pcputimes(ct)
        },

        create_time = function() {
          if (self$.pid == 0L || self$.pid == 4L) {
            ps_boot_time()
          } else {
            tryCatch(
              .Call(ps__proc_create_time, self$.pid),
              error = function(e) {
                ct <- self$.oneshot_info()[["create_time"]]
                as.POSIXct(ct, origin = "1970-01-01", tz = "GMT")
              })
          }
        },

        num_threads = function() {
          self$.oneshot_info()[["num_threads"]]
        },

        suspend = function() {
          self$.assert_pid_not_reused()
          ## TODO
        },

        resume = function() {
          self$.assert_pid_not_reused()
          ## TODO
        },

        kill = function() {
          self$.assert_pid_not_reused()
          ## TODO
        },

        .oneshot_info = function() {
          .Call(ps__proc_info, self$.pid)
        },

        .raw_meminfo = function() {
          tryCatch(
            .Call(ps__proc_memory_info, self$.pid),
            error = function(e) {
              info <- self$.oneshot_info()
              info[c("num_page_faults", "peak_wset", "wset", "peak_paged_pool",
                     "paged_pool", "peak_non_paged_pool", "non_paged_pool",
                     "pagefile", "peak_pagefile", "mem_private")]
            })
        }
      )
    )
  }

  ps_env$process_windows
}

#' @importFrom utils head

convert_dos_path <- function(path) {
  pcs <- strsplit(path, "\\", fixed =  TRUE)[[1]]
  rawdrive <- paste(head(pcs, 3), collapse = "\\")
  driveletter <- .Call(ps__win32_QueryDosDevice, rawdrive)
  paste0(driveletter, substr(path, nchar(rawdrive) + 1, nchar(path)))
}

ps_ppid_map_windows <- function() {
  pids <- .Call(ps__ppid_map)
  pidx <- seq(2L, length(pids), by = 2L)
  data.frame(
    pid = pids[pidx],
    ppid = pids[pidx-1L]
  )
}
