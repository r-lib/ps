
#' Send an alert to the window of a process
#'
#' This function currently only works on Windows and errors on
#' other platforms.
#'
#' R processes might not have an associated window, e.g. if they are
#' running in a terminal, or in RStudio. In RGui they do.
#'
#' @param p Process handle.
#' @param ancestors Logical flag. Whether to try to alert the
#' closest ancestor in the process tree, if the specified process
#' does not have an associated window. It uses [ps_descent()] to
#' look up the ancestors.
#' @return A named list:
#' * `proc`: a process handle. This is the handle that ps tried to
#'   alert. It might that same as `p` or an ancestor, if
#'   `ancestors = TRUE` was specified. This is `NULL` if ps did
#'   not find any process with an associated Window to alert.
#' * `success`: whether the Windows API call returned success.
#'   If this is `TRUE` that typically means that the alerted
#'   window is the active one. If it is `FALSE`, then the alert was
#'   probably still sent, and the user will see it on the status bar
#'   (in Windows 10).
#'
#' @export
#' @examples
#' \dontrun{
#' # This usually does nothing interactively, since the current
#' # (RStudio, RGui, Windows Terminal, etc.) window is on top.
#' ps_switch_to()
#'
#' # Try switching to another window, while the sleep is running,
#' # and then you'll see an alert for the current
#' # (RStudio, RGui, etc.) window
#' Sys.sleep(4); ps_switch_to()
#' }

ps_switch_to <- function(p = ps_handle(), ancestors = TRUE) {
  os <- ps_os_type()
  if (!os[["WINDOWS"]]) {
    stop("`ps_windows()` currently only works on Windows")
  }
  assert_ps_handle(p)

  if (ancestors) {
    plist <- ps_descent(p)
  } else {
    plist <- list(p)
  }
  pids <- map_int(plist, ps_pid)

  ret <- .Call(psll_switch_to, pids)
  if (ret == 0) {
    list(proc = NULL, success = FALSE)
  } else {
    # -pid on failure
    list(proc = plist[[match(abs(ret), pids)]], success = ret > 0)
  }
}
