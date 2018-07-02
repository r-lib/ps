
get_procfs_path <- function() {
  os <- ps_os_type()
  if (os[["LINUX"]] || os[["SUNOS"]] || os[["AIX"]]) {
      "/proc"
  }
}

#' @importFrom R6 R6Class

process_posix <- function() {
  if (is.null(ps_env$process_posix)) {
    ps_env$process_posix <- R6Class(
      "process_posix",
      cloneable = FALSE,
      inherit = process_common(),
      public = list(

        name = function() {
          ## Process name is only cached on Windows as on POSIX it may
          ## change, see:
          ## https://github.com/giampaolo/psutil/issues/692
          n = self$.proc_name()
          if (nchar(n) >= 15) {
            ## On UNIX the name gets truncated to the first 15 characters.
            ## If it matches the first part of the cmdline we return that
            ## one instead because it's usually more explicative.
            ## Examples are "gnome-keyring-d" vs. "gnome-keyring-daemon".
            cmdline <- tryCatch(
              self$cmdline(),
              access_denied = function(e) NULL
            )
            if (!is.null(cmdline)) {
              exname <- basename(cmdline[1])
              if (str_starts_with(exname, n)) n <- exname
            }
          }
          self$.name <- n
          n
        },

        username = function() {
          real_uid <- self$uids()[["real"]]
          tryCatch(
            get_pw_uid(real_uid)[["pw_name"]],
            ## the uid can't be resolved by the system
            error = function(e) as.character(real_uid)
          )
        },

        send_signal = decorator(assert_pid_not_reused, function(sig) {
          self$.send_signal(sig)
        }),

        suspend = decorator(assert_pid_not_reused, function() {
          self$.send_signal(signals()$SIGSTOP)
        }),

        resume = decorator(assert_pid_not_reused, function() {
          self$.send_signal(signals()$SIGCONT)
        }),

        terminate = decorator(assert_pid_not_reused, function() {
          self$send_signal(signals()$SIGTERM)
        }),

        kill = decorator(assert_pid_not_reused, function() {
          self$send_signal(signals()$SIGKILL)
        }),

        .common_puids = function(values) {
          values <- as.integer(values)
          names(values) <- c("real", "effective", "saved")
          values
        },

        .send_signal = function(sig) {
          assert_that(self$.pid >= 0L)
          if (self$.pid == 0L) {
            stop("preventing sending signal to process with PID 0 as it ",
                 "would affect every process in the process group of the ",
                 "calling process (Sys.getpid()) instead of PID 0")
          }
          tryCatch(
            .Call(psp__kill, self$.pid, sig),
            os_error = function(e) {
              if (e$errno == errno()$ESRCH) {
                stop(ps__no_such_process(self$.pid, self$.name))
              } else if (e$errno == errno()$EPERM ||
                         e$errno == errno()$EACCES) {
                stop(ps__access_denied(self$.pid, self$.name))
              } else {
                stop(e)
              }
            }
          )
        }
      )
    )
  }

  ps_env$process_posix
}

get_pw_uid <- function(uid) {
  .Call(psp__get_pw_uid, as.integer(uid))
}

signals <- function() {
  ps_env$constants$signals
}

errno <- function() {
  ps_env$constants$errno
}

get_terminal_map <- function() {
  ls <- c(
    dir("/dev", pattern = "^tty.*", full.names = TRUE),
    dir("/dev/pts", full.names = TRUE))
  ret <- structure(ls, names = as.character(.Call(psp__stat_st_rdev, ls)))
  ret[names(ret) != "0"]
}
