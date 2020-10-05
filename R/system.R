
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
#' @examplesIf ps::ps_is_supported()
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

#' @export

ps_disk_partitions <- function(all = FALSE) {
  assert_flag(all)
  l <- not_null(.Call(ps__disk_partitions, all))

  d <- data.frame(
    stringsAsFactors = FALSE,
    device = vapply(l, "[[", character(1), 1),
    mountpoint = vapply(l, "[[", character(1), 2),
    fstype = vapply(l, "[[", character(1), 3),
    options = vapply(l, "[[", character(1), 4)
  )

  if (!all) d <- ps__disk_partitions_filter(d)

  requireNamespace("tibble", quietly = TRUE)
  class(d) <- unique(c("tbl_df", "tbl", class(d)))
  d
}

ps__disk_partitions_filter <- function(pt) {
  os <- ps_os_name()

  if (os == "LINUX") {
    fs <- read.delim("/proc/filesystems", header = FALSE, sep = "\t")
    goodfs <- c(fs[[2]][fs[[1]] != "nodev"], "zfs")
    ok <- pt$device != "none" & file.exists(pt$device) & pt$fstype %in% goodfs
    ok <- ok | pt$device %in% c("overlay", "grpcfuse")
    pt <- pt[ok, , drop = FALSE]

  } else if (os == "MACOS") {
    ok <- substr(pt$device, 1, 1) == "/" & file.exists(pt$device)
    pt <- pt[ok, , drop = FALSE]
  }

  pt
}

#' @export

ps_disk_usage <- function(paths = ps_disk_partitions()$mountpoint) {
  assert_character(paths)
  l <- .Call(ps__disk_usage, paths)
  os <- ps_os_name()
  if (os == "WINDOWS") {
    ps__disk_usage_format_windows(paths, l)
  } else {
    ps__disk_usage_format_posix(paths, l)
  }
}

ps__disk_usage_format_windows <- function(paths, l) {
  total <- vapply(l, "[[", double(1), 1)
  free <- vapply(l, "[[", double(1), 2)
  freeuser <- vapply(l, "[[", double(1), 3)
  used <- total - free

  d <- data.frame(
    stringsAsFactors = FALSE,
    mountpoint = paths,
    total = total,
    used = used,
    available = freeuser,
    capacity = used / total
  )

  requireNamespace("tibble", quietly = TRUE)
  class(d) <- unique(c("tbl_df", "tbl", class(d)))
  d
}

ps__disk_usage_format_posix <- function(paths, l) {
  l2 <- lapply(l, function(fs) {
    total <- fs[[5]] * fs[[1]]
    avail_to_root <- fs[[6]] * fs[[1]]
    avail = fs[[7]] * fs[[1]]
    used  <-  total - avail_to_root
    total_user <- used + avail
    usage_percent <- used / total_user
    list(total = total, used = used, free = avail, percent = usage_percent)
  })

  d <- data.frame(
    stringsAsFactors = FALSE,
    mountpoint = paths,
    total = vapply(l2, "[[", double(1), "total"),
    used = vapply(l2, "[[", double(1), "used"),
    available = vapply(l2, "[[", double(1), "free"),
    capacity = vapply(l2, "[[", double(1), "percent")
  )

  requireNamespace("tibble", quietly = TRUE)
  class(d) <- unique(c("tbl_df", "tbl", class(d)))
  d
}
