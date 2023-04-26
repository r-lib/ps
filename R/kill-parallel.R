#' Kill processes with grace period
#'
#' On Unixes, `ps_kill_parallel()` sends a `SIGTERM` to all processes
#' in `ps`. If the processes are not killed after a grace period
#' defined by `grace`, a `SIGKILL` is issued. On Windows, the
#' processes are abruptly terminated without grace period.
#'
#' Direct subprocesses are polled for termination events, which means
#' that `ps_kill_parallel()` will return earlier than the grace period
#' as soon as all `SIGTERM` signals are successful. However if `ps`
#' includes subsubprocesses, those are periodically polled for
#' termination as ps doesn't receive termination events for those. In
#' this case, `ps_kill_parallel()` might return a little later.
#'
#' @param ps List of [process handles](ps_handle).
#' @param grace Grace period between `SIGTERM` and `SIGKILL` in seconds.
#' @export
ps_kill_parallel <- function(ps, grace = 0.2) {
  stopifnot(
    is_handle_list(ps),
    is_scalar_numeric(grace)
  )

  if (.Platform$OS == "windows") {
    for (p in ps) {
      ps_kill(p)
    }
  } else {
    .Call(ps__kill_parallel, ps, grace)
  }

  NULL
}
