
#' List all mounted partitions
#'
#' The output is similar the Unix `mount` and `df` commands.
#'
#' @param all Whether to list virtual devices as well. If `FALSE`, on
#' Linux it will still list `overlay` and `grpcfuse` file systems, to
#' provide some useful information in Docker containers.
#' @return A data frame with columns `device`, `mountpoint`,
#' `fstype` and `options`.
#'
#' @family disk functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_disk_partitions(all = TRUE)
#' ps_disk_partitions()

ps_disk_partitions <- function(all = FALSE) {
  assert_flag(all)
  l <- not_null(.Call(ps__disk_partitions, all))

  d <- data_frame(
    device = vapply(l, "[[", character(1), 1),
    mountpoint = vapply(l, "[[", character(1), 2),
    fstype = vapply(l, "[[", character(1), 3),
    options = vapply(l, "[[", character(1), 4)
  )

  if (!all) d <- ps__disk_partitions_filter(d)

  d
}

#' @importFrom utils read.delim

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

#' Disk usage statistics, per partition
#'
#' The output is similar to the Unix `df` command.
#'
#'
#' Note that on Unix a small percentage of the disk space (5% typically)
#' is reserved for the superuser. `ps_disk_usage()` returns the space
#' available to the calling user.
#'
#' @param paths The mounted file systems to list. By default all file
#' systems returned by [ps_disk_partitions()] is listed.
#' @return A data frame with columns `mountpoint`, `total`, `used`,
#' `available` and `capacity`.
#'
#' @family disk functions
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' ps_disk_usage()


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

  d <- data_frame(
    mountpoint = paths,
    total = total,
    used = used,
    available = freeuser,
    capacity = used / total
  )

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

  d <- data_frame(
    mountpoint = paths,
    total = vapply(l2, "[[", double(1), "total"),
    used = vapply(l2, "[[", double(1), "used"),
    available = vapply(l2, "[[", double(1), "free"),
    capacity = vapply(l2, "[[", double(1), "percent")
  )

  d
}


#' System-wide disk I/O counters
#'
#' Returns a data.frame of system-wide disk I/O counters.
#'
#' Includes the following fields for all supported platforms:
#' * `read_count`: number of reads
#' * `write_count`: number of writes
#' * `read_bytes`: number of bytes read
#' * `write_bytes`: number of bytes written
#'
#' And for only some platforms:
#' * `read_time`: time spent reading from disk (in milliseconds)
#' * `write_time`: time spent writing to disk (in milliseconds)
#' * `busy_time`: time spent doing actual I/Os (in milliseconds)
#' * `read_merged_count`: number of merged reads (see iostats doc)
#' * `write_merged_count`: number of merged writes (see iostats doc)
#'
#' @param perdisk If `TRUE`, return 1 row for every physical disk on the system, else
#' return totals.
#'
#' @return A data frame of either 1 row of total disk I/O, or 1 row
#' per disk of I/O stats, with columns `name`, `read_count` `read_merged_count`
#' `read_bytes`, `read_time`, `write_count`, `write_merged_count`, `write_bytes`
#' `write_time`, and `busy_time`. `name` will be `NA` if perdisk is `FALSE`.
#'
#' @family disk functions
#' @export
#' @examplesIf ps:::ps_os_name() == "LINUX" && ! ps:::is_cran_check()
#' ps_disk_io_counters()
#' ps_disk_io_counters(perdisk = TRUE)
ps_disk_io_counters <- function(perdisk = FALSE) {
  assert_flag(perdisk)
  if (ps_os_name() == "LINUX") {
    return(ps__disk_io_counters_linux(perdisk))
  } else {
    stop("ps_disk_io_counters is currently only implemented for Linux")
  }
}
