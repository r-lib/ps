
#' @export

ps_mark_tree <- function() {
  id <- ps_env$marker <- ps_env$marker %||% get_id()
  do.call(Sys.setenv, structure(list("YES"), names = id))
  id
}

#' @export

ps_kill_tree <- function(marker = NULL, exclude_me = TRUE,
                         sig = signals()$SIGKILL) {
  marker <- marker %||% ps_env$marker
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

  ## Otherwise try to clean them up, carefully, to minimize racing
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
    ret <- c(ret, list(structure(p, names = info$name)))
  }

  ## This works for empty lists as well, and keeps names
  ret <- unlist(not_null(ret))
  if (length(ret)) ret else structure(integer(), names = character())
}

ps_kill_tree_linux <- function(marker, exclude_me, sig) {
  stop(ps__not_implememnted())
}

ps_kill_tree_windows <- function(maker, exclude_me) {
  stop(ps__not_implememnted())
}
