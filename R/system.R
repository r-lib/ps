
#' Ids of all processes on the system
#'
#' @return Integer vector of process ids.
#' @export

ps_pids <- function() {
  os <- ps_os_type()
  pp <- if (os[["MACOS"]])
    ps_pids_macos()
  else if (os[["LINUX"]])
    ps_pids_linux()
  else if (os[["WINDOWS"]])
    ps_pids_windows()
  else
    stop("Not implemented for this platform")

  sort(pp)
}

ps_pids_windows <- function() {
  sort(.Call(ps__pids))
}

ps_pids_macos <- function() {
  ls <- .Call(ps__pids)
  ## 0 is missing from the list, usually, even though it is a process
  if (! 0L %in% ls && ps_pid_exists_macos(0L)) {
    ls <- c(ls, 0L)
  }
  ls
}

ps_pid_exists_macos <- function(pid) {
  .Call(psp__pid_exists, as.integer(pid))
}

ps_pids_linux <- function() {
  sort(as.integer(dir("/proc", pattern = "^[0-9]+$")))
}

#' Boot time of the system
#'
#' @return A `POSIXct` object.
#'
#' @export

ps_boot_time <- function() {
  format_unix_time(.Call(ps__boot_time))
}

#' List users connected to the system
#'
#' @return A data frame (tibble) with columns
#'  `username`, `tty`, `hostname`, `start_time`, `pid`. `tty` and `pid`
#'  are `NA` on Windows. `pid` is the process id of the login process.
#'  For local users the `hostname` column is the empty string.
#'
#' @export

ps_users <- function() {
  l <- not_null(.Call(ps__users))

  d <- data.frame(
    stringsAsFactors = FALSE,
    username = vapply(l, "[[", character(1), 1),
    tty = vapply(l, "[[", character(1), 2),
    hostname = vapply(l, "[[", character(1), 3),
    start_time = format_unix_time(vapply(l, "[[", double(1), 4)),
    pid = vapply(l, "[[", integer(1),  5)
  )

  requireNamespace("tibble", quietly = TRUE)
  class(d) <- unique(c("tbl_df", "tbl", class(d)))
  d
}

#' Number of logical or physical CPUs
#'
#' If cannot be determined, it returns `NA`. It also returns `NA` on older
#' Windows systems, e.g. Vista or older and Windows Server 2008 or older.
#'
#' @param logical Whether to count logical CPUs.
#' @return Integer scalar.
#'
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_cpu_count(logical = TRUE)
#' ps_cpu_count(logical = FALSE)

ps_cpu_count <- function(logical = TRUE) {
  assert_flag(logical)
  if (logical) ps_cpu_count_logical() else ps_cpu_count_physical()
}

 ps_cpu_count_logical <- function() {
   .Call(ps__cpu_count_logical)
 }

ps_cpu_count_physical <- function() {
  if (ps_os_type()[["LINUX"]]) {
    ps_cpu_count_physical_linux()
  } else {
    .Call(ps__cpu_count_physical)
  }
}

#' Query the size of the current terminal
#'
#' If the standard output of the current R process is not a terminal,
#' e.g. because it is redirected to a file, or the R process is running in
#' a GUI, then it will throw an error. You need to handle this error if
#' you want to use this function in a package.
#'
#' If an error happens, the error message is different depending on
#' what type of device the standard output is. Some common error messages
#' are:
#' * "Inappropriate ioctl for device."
#' * "Operation not supported on socket."
#' * "Operation not supported by device."
#'
#' Whatever the error message, `ps_tty_size` always fails with an error of
#' class `ps_unknown_tty_size`, which you can catch.
#'
#' @export
#' @examples
#' # An example that falls back to the 'width' option
#' tryCatch(
#'   ps_tty_size(),
#'   ps_unknown_tty_size = function(err) {
#'     c(width = getOption("width"), height = NA_integer_)
#'   }
#' )

ps_tty_size <- function() {
  tryCatch(
    ret <- .Call(ps__tty_size),
    error = function(err) {
      class(err) <- c("ps_unknown_tty_size", class(err))
      stop(err)
    }
  )
  c(width = ret[1], height = ret[2])
}
