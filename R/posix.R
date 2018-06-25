
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
            get_pwd_uid(real_uid)[["pw_name"]],
            ## the uid can't be resolved by the system
            error = function(e) as.character(real_uid)
          )
        },

        send_signal = function(sig) {
          self$.assert_pid_not_reused()
          self$.send_signal(sig)
        },

        suspend = function() {
          self$.assert_pid_not_reused()
          self$.send_signal(signals()$SIGSTOP)
        },

        resume = function() {
          self$.assert_pid_not_reused()
          self$.send_signal(signals()$SIGCONT)
        },

        terminate = function() {
          self$.assert_pid_not_reused()
          self$send_signal(signals()$SIGTERM)
        },

        kill = function() {
          self$.assert_pid_not_reused()
          self$send_signal(signals()$SIGKILL)
        },

        .common_puids = function(values) {
          values <- as.integer(values)
          names(values) <- c("real", "effective", "saved")
          values
        },

        .send_signal = function(sig) {
          ##  TODO
        }
      )
    )
  }

  ps_env$process_posix
}

get_pwd_uid <- function(uid) {
  ## TODO
}

signals <- function() {
  list(
    SIGHUP = 1,
    SIGINT = 2,
    SIGQUIT = 3,
    SIGILL = 4,
    SIGTRAP = 5,
    SIGABRT = 6,
    SIGEMT = 7,
    SIGFPE = 8,
    SIGKILL = 9,
    SIGBUS = 10,
    SIGSEGV = 11,
    SIGSYS = 12,
    SIGPIPE = 13,
    SIGALRM = 14,
    SIGTERM = 15,
    SIGURG = 16,
    SIGSTOP = 17,
    SIGTSTP = 18,
    SIGCONT = 19,
    SIGCHLD = 20,
    SIGTTIN = 21,
    SIGTTOU = 22,
    SIGIO = 23,
    SIGXCPU = 24,
    SIGXFSZ = 25,
    SIGVTALRM = 26,
    SIGPROF = 27,
    SIGWINCH = 28,
    SIGINFO = 29,
    SIGUSR1 = 30,
    SIGUSR2 = 31
  )
}
