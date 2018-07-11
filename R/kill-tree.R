
#' Mark a process and its (future) child tree
#'
#' `ps_mark_tree()` generates a random environment variable name and sets
#' it in the  current R process. This environment variable will be (by
#' default) inherited by all child (and grandchild, etc.) processes, and
#' will help finding these processes, even if and when they are (no longer)
#' related to the current R process. (I.e. they are not connected in the
#' process tree.)
#'
#' `ps_kill_tree()` finds the processes that set the supplied environment
#' variable, and kills them (or sends them the specified signal on Unix).
#'
#' `with_process_cleanup()` evaluates an R expression, and cleans up all
#' external processes that were started by the R process while evaluating
#' the expression. This includes child processes of child processes, etc.,
#' recursively.
#'
#' @return `ps_mark_tree()` returns the name of the environment variable,
#' which can be used as the `marker` in `ps_kill_tree()`.
#'
#' `ps_kill_tree()` returns the pids of the killed processes, in a named
#' integer vector. The names are the file names of the executables, when
#' available.
#'
#' `with_process_cleanup()` returns the value of the evaluated expression.
#'
#' @rdname ps_kill_tree
#' @export

ps_mark_tree <- function() {
  id <- get_id()
  do.call(Sys.setenv, structure(list("YES"), names = id))
  id
}

#' @param expr R expression to evaluate in the new context.
#'
#' @rdname ps_kill_tree
#' @export

with_process_cleanup <- function(expr) {
  id <- ps_mark_tree()
  on.exit(ps_kill_tree(id), add = TRUE)
  expr
}

#' @param marker String scalar, the name of the environment variable to
#' use to find the marked processes.
#' @param exclude_me If `TRUE`, then the calling process is not killed,
#' even if it sets the `marked` environment variable.
#' @param sig The signal to send to the marked processes on Unix. On
#' Windows this argument is ignored currently.
#'
#' @rdname ps_kill_tree
#' @export

ps_kill_tree <- function(marker, exclude_me = TRUE,
                         sig = signals()$SIGKILL) {

  assert_that(is_string(marker), is_flag(exclude_me))

  osname <- ps_os_name()
  if (is.na(osname)) stop("Unsupported platform")

  switch(
    osname,
    MACOS = ps_kill_tree_macos(marker, exclude_me, sig),
    LINUX = ps_kill_tree_linux(marker, exclude_me, sig),
    WINDOWS = ps_kill_tree_windows(marker, exclude_me)
  )
}

ps_kill_tree_macos <- function(marker, exclude_me, sig) {

  ## Get all process environments
  pids <- .Call(ps__pids)
  envs <- lapply(pids, function(p) {
    tryCatch(
      .Call(ps__proc_environ, p),
      no_such_process = function(e) NULL,
      access_denied = function(e) NULL
    )
  })

  ## Find the ones that are marked
  match <- which(map_int(envs, function(x) length(grep(marker, x))) > 0)
  cand <- pids[match]

  ## Exclude myself
  cand <- setdiff(cand, Sys.getpid())

  ## Try to clean them up, carefully, to minimize racing
  ret <- lapply(cand, function(p) {
    tryCatch({
      info <- .Call(ps__proc_kinfo_oneshot, p)
      env <- .Call(ps__proc_environ, p)
      if (length(grep(marker, env))) {
        .Call(ps__kill, p, sig)
        structure(p, names = info$name)
      } },
      no_such_process = function(e) NULL,
      access_denied = function(e) NULL
    )
  })

  if (!exclude_me && Sys.getenv(marker) != "") {
    mypid <- Sys.getpid()
    .Call(ps__kill, mypid, sig)
    info <- .Call(ps__proc_kinfo_oneshot, mypid)
    ret <- c(ret, list(structure(mypid, names = info$name)))
  }

  ## This works for empty lists as well, and keeps names
  ret <- unlist(not_null(ret))
  if (length(ret)) ret else structure(integer(), names = character())
}

ps_kill_tree_linux <- function(marker, exclude_me, sig) {

  ## Match process environments
  pids <- ps_pids_linux()
  match <- map_lgl(pids, function(p) {
    tryCatch(
      data <- .Call(psl__linux_match_environ, "/proc", marker, p),
      error = function(e) NULL
    )
  })
  cand <- pids[match]

  ## Exclude myself
  cand <- setdiff(cand, Sys.getpid())

  ## Try to clean them up, carefully, to minimize racing
  ret <- lapply(cand, function(p) {
    tryCatch({
      nm <- ps_name(ps_handle(p))
      rv <- .Call(psl__kill_tree_process, "/proc", marker, p, sig)
      if (!is.null(rv)) structure(p, names = nm) },
      error = function(e) NULL
    )
  })

  if (!exclude_me && Sys.getenv(marker) != "") {
    mypid <- Sys.getpid()
    .Call(ps__kill, mypid, sig)
    me <- ps_handle(mypid)
    ret <- c(ret, list(structure(mypid, names = ps_name(me))))
  }

  ## This works for empty lists as well, and keeps names
  ret <- unlist(not_null(ret))
  if (length(ret)) ret else structure(integer(), names = character())
}

ps_kill_tree_windows <- function(marker, exclude_me) {

  ## Get all process environments
  pids <- .Call(ps__pids)
  envs <- lapply(pids, function(p) {
    tryCatch(
      .Call(ps__proc_environ, p),
      error = function(e) NULL
    )
  })

  ## Find the ones that are marked
  match <- which(map_int(envs, function(x) length(grep(marker, x))) > 0)
  cand <- pids[match]

  ## Exclude myself
  cand <- setdiff(cand, Sys.getpid())

  ## Try to clean them up, carefully, to minimize racing
  ret <- lapply(cand, function(p) {
    name <- tryCatch(
      basename(convert_dos_path(.Call(ps__proc_exe, p))),
      error = function(e) "???"
    )
    tryCatch({
      rv <- .Call(ps__kill_tree_process, marker, p)
      if (!is.null(rv)) structure(p, names = name) },
      error = function(e) NULL
    )
  })

  if (!exclude_me && Sys.getenv(marker) != "") {
    mypid <- Sys.getpid()
    .Call(ps__proc_kill, mypid)
    me <- ps_handle(mypid)
    ret <- c(ret, list(structure(mypid, names = ps_name(me))))
  }

  ## This works for empty lists as well, and keeps names
  ret <- unlist(not_null(ret))
  if (length(ret)) ret else structure(integer(), names = character())
}
