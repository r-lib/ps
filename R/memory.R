
#' Statistics about system memory usage
#'
#' @return List with entries. All numbers are in bytes:
#' * `total`: total physical memory (exclusive swap).
#' * `avail` the memory that can be given instantly to processes without
#'   the system going into swap. This is calculated by summing different
#'   memory values depending on the platform and it is supposed to be used
#'   to monitor actual memory usage in a cross platform fashion.
#' * `percent`: Percentage of memory that is taken.
#' * `used`: memory used, calculated differently depending on
#'   the platform and designed for informational purposes only.
#'   `total` - `free` does not necessarily match `used`.
#' * `free`: memory not being used at all (zeroed) that is
#'   readily available; note that this doesn’t reflect the actual memory
#'   available (use `available` instead). `total` - `used` does not
#'   necessarily match `free`.
#' * `active`: (Unix only) memory currently in use or very recently used,
#'   and so it is in RAM.
#' * `inactive`: (Unix only) memory that is marked as not used.
#' * `wired`: (macOS only) memory that is marked to always stay in RAM. It
#'   is never moved to disk.
#'
#' @family memory functions
#' @export
#' @examplesIf ps::ps_is_supported()
#' ps_system_memory()

ps_system_memory <- function() {
  l <- .Call(ps__system_memory)
  os <- ps_os_name()

  if (os == "MACOS") {
    l$avail <- l$inactive + l$free
    l$used <- l$active + l$wired
    l$free <- l$free - l$speculative
    l$percent <- (l$total - l$avail) / l$total
    l[c("total", "avail", "percent", "used", "free",
        "active", "inactive", "wired")]

  } else if (os == "LINUX") {

  } else if (os == "WINDOWS") {

  }
}

#' @export

ps_system_swap <- function() {
  .Call(ps__system_swap)
}
