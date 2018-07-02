ps_pids_linux <- function() {
  sort(as.integer(dir(get_procfs_path(), pattern =  "^[0-9]+$")))
}

ps_pid_exists_linux <- function(pid) {
  if (! .Call(psp__pid_exists, as.integer(pid)))  {
    FALSE
  } else {
    ## Linux's apparently does not distinguish between PIDs and TIDs
    ## (thread IDs).
    ## listdir("/proc") won't show any TID (only PIDs) but
    ## os.stat("/proc/{tid}") will succeed if {tid} exists.
    ## os.kill() can also be passed a TID. This is quite confusing.
    ## In here we want to enforce this distinction and support PIDs
    ## only, see:
    ## https://github.com/giampaolo/psutil/issues/687
    tryCatch({
      ## Note: already checked that this is faster than using a
      ## regular expr. Also (a lot) faster than doing
      ## 'return pid in pids()'
      path <- sprintf("%s/%i/status", get_procfs_path(), pid)
      lines <- read_lines(path)
      tgidline <- grep("^Tgid:", lines, value = TRUE)[1]
      tgid <- strsplit(tgidline, "\\s+")[[1]][2]
      identical(as.integer(tgid), as.integer(pid))
    },
    error = function() {
        pid %in% ps_pids()
    })
  }
}

ps_boot_time_raw_linux <- function() {
  if (is.null(ps_env$boot_time)) {
    path <- sprintf("%s/stat", get_procfs_path())
    btime <- grep("btime", read_lines(path), value = TRUE)
    ps_env$boot_time <-
      as.numeric(strsplit(str_strip(btime), "\\s+")[[1]][[2]])
  }
  ps_env$boot_time
}

linux_wrap_exceptions <- function(fun) {
  fun
  function(...) {
    tryCatch(
      fun(...),
      error = function(e) {
        path <- sprintf("%s/%i", get_procfs_path(), self$.pid)
        if (file.exists(path)) {
          stop(ps__access_denied(self$.pid, self$.name))
        } else {
          stop(ps__no_such_process(self$.pid, self$.name))
        }
      }
    )
  }
}

#' @importFrom R6 R6Class

process_linux <- function() {
  if (is.null(ps_env$process_linux)) {
    ps_env$process_linux <- R6Class(
      "process_linux",
      cloneable = FALSE,
      inherit = process_posix(),
      public = list(

        name = decorator(linux_wrap_exceptions, function() {
            self$.parse_stat_file()[[1]]
        }),

        exe = function() {
          tryCatch(
            readlink(sprintf("%s/%i/exe", get_procfs_path(), self$.pid)),
            os_error = function(e) {
              if (e$errno == errno()$ENOENT ||
                  e$errno == errno()$ESRCH) {
                ## no such file error; might be raised also if the
                ## path actually exists for system processes with
                ## low pids (about 0-20)
                path <- sprintf("%s/%i", get_procfs_path(), self$.pid)
                if (file.exists(path)) return("")
                if (!ps_pid_exists(self$.pid)) {
                  stop(ps__no_such_process(self$.pid, self$.name))
                } else {
                  stop(ps__zombie_process(self$.pid, self$.name, self$.ppid))
                }
              }
              if (e$errno() == errno()$EPERM ||
                  e$errno() == errno()$EACCES) {
                stop(ps__access_denied(self$.pid, self$.name))
              }
            })
        },

        cmdline = decorator(linux_wrap_exceptions, function() {
          path <- sprintf("%s/%i/cmdline", get_procfs_path(), self$.pid)
          data <- read_binary_file(path)

          ## May happen in case of a zombie process
          if (!length(data)) return(character())

          ## 'man proc' states that args are separated by null bytes '\0'
          ## and last char is supposed to be a null byte. Nevertheless
          ## some processes may change their cmdline after being started
          ## (via setproctitle() or similar), they are usually not
          ## compliant with this rule and use spaces instead. Google
          ## Chrome process is an example. See:
          ## https://github.com/giampaolo/psutil/issues/1179
          sep <-  if (data[length(data)] == 0x00) 0x00 else charToRaw(" ")
          map_chr(raw_split(data[-length(data)], sep), rawToChar)
        }),

        environ = decorator(linux_wrap_exceptions, function() {
          path <- sprintf("%s/%i/environ", get_procfs_path(), self$.pid)
          data <- read_binary_file(path)
          parse_envs(map_chr(raw_split(data[-length(data)], 0x00), rawToChar))
        }),

        ppid = decorator(linux_wrap_exceptions, function() {
          as.integer(self$.parse_stat_file()[[3]])
        }),

        cwd = decorator(linux_wrap_exceptions, function() {
          readlink(sprintf("%s/%i/cwd", get_procfs_path(), self$.pid))
        }),

        uids = decorator(linux_wrap_exceptions, function() {
          status <- self$.read_status_file()
          line <- grep("^Uid:", status, value = TRUE)[1]
          match <- re_match(line, "^Uid:\\t(\\d+)\\t(\\d+)\\t(\\d+)")
          self$.common_puids(c(match[[1]], match[[2]], match[[3]]))
        }),

        gids = decorator(linux_wrap_exceptions, function() {
          status <- self$.read_status_file()
          line <- grep("^Gid:", status, value = TRUE)[1]
          match <- re_match(line, "^Gid:\\t(\\d+)\\t(\\d+)\\t(\\d+)")
          self$.common_puids(c(match[[1]], match[[2]], match[[3]]))
        }),

        memory_info = decorator(linux_wrap_exceptions, function() {
          path <- sprintf("%s/%i/statm", get_procfs_path(), self$.pid)
          mi <- scan(path, n = 7, quiet = TRUE)
          names(mi) <- c("rss", "vms", "shared", "text", "lib", "data",
                         "dirty")
          mi
        }),

        cpu_times = decorator(linux_wrap_exceptions, function() {
          stat <- self$.parse_stat_file()
          self$.common_pcputimes(
                 as.numeric(stat[c(13:16)]) / linux_clock_ticks())
        }),

        .create_time_raw = decorator(linux_wrap_exceptions, function() {
          stat <- self$.parse_stat_file()
          bt <- ps_boot_time_raw()
          bt + as.numeric(stat[[21]]) / linux_clock_ticks()
        }),

        num_threads = decorator(linux_wrap_exceptions, function() {
          status <- self$.read_status_file()
          line <- grep("^Threads:", status, value = TRUE)[1]
          match <- re_match(line, "^Threads:\\t(\\d+)")
          as.integer(match[[1]])
        }),

        terminal = decorator(linux_wrap_exceptions, function() {
          num <- self$.parse_stat_file()[[6]]
          tmap <- get_terminal_map()
          tmap[[as.character(num)]]
        }),

        status = decorator(linux_wrap_exceptions, function() {
          letter <- self$.parse_stat_file()[[2]]
          self$.proc_statuses()[[letter]]
        }),

        ## Internal methods
        .parse_stat_file = decorator(memoize_when_activated, function() {
          path <- sprintf("%s/%i/stat", get_procfs_path(), self$.pid)
          stat <- paste(read_lines(path), collapse = "\n")
          name <- sub("^.*[(](.*)[)].*$", "\\1", stat, perl = TRUE)
          fields <- strsplit(sub("^.*[)]\\s+", "", stat), "\\s+")[[1]]
          c(name, fields)
        }),

        .read_status_file = decorator(memoize_when_activated, function()  {
          path <-  sprintf("%s/%i/status", get_procfs_path(), self$.pid)
          read_lines(path)
        }),

        .proc_statuses = function() {
          c("R" = "running",
            "S" = "sleeping",
            "D" = "disk_sleep",
            "T" = "stopped",
            "t" = "tracing_stop",
            "Z" = "zombie",
            "X" = "dead",
            "x" = "dead",
            "K" = "wake_kill",
            "W" = "waking")
        },

        .oneshot_enter = function() {
          super$.oneshot_enter()
          self$.parse_stat_file$activate()
          self$.read_status_file$activate()
        },

        .oneshot_exit = function() {
          super$.oneshot_exit()
          self$.parse_stat_file$deactivate()
          self$.read_status_file$deactivate()
        }
      )
    )
  }

  ps_env$process_linux
}

linux_clock_ticks <- function() {
  if (is.null(ps_env$clock_ticks)) {
    ps_env$clock_ticks <- .Call(psl__linux_clk_tck)
  }
  ps_env$clock_ticks
}

linux_pagesize <- function() {
  if (is.null(ps_env$pagesize)) {
    ps_env$pagesie <- .Call(psl__linux_pagesize)
  }
  ps_env$pagesize
}
