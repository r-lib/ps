
#' @export

ps_handle <- function(pid = NULL, time = NULL) {
  .Call(psll_handle, pid, time)
}

#' @export

format.ps_handle <- function(x, ...) {
  pieces <- .Call(psll_format, x)
  paste0("<ps::ps_handle> PID=", pieces[[2]], ", NAME=", pieces[[1]],
         ", AT=", format_unix_time(pieces[[3]]))
}

#' @export

print.ps_handle <- function(x, ...)  {
  cat(format(x, ...),  "\n", sep = "")
}

#' @export

ps_pid <- function(p) {
  .Call(psll_pid, p)
}

#' @export

ps_parent <- function(p) {
  .Call(psll_parent, p)
}

#' @export

ps_ppid <- function(p) {
  .Call(psll_ppid, p)
}

#' @export

ps_is_running <- function(p) {
  .Call(psll_is_running, p)
}

#' @export

ps_name <- function(p) {
  n <- .Call(psll_name, p)
  if (nchar(n) >= 15) {
    ## On UNIX the name gets truncated to the first 15 characters.
    ## If it matches the first part of the cmdline we return that
    ## one instead because it's usually more explicative.
    ## Examples are "gnome-keyring-d" vs. "gnome-keyring-daemon".
    cmdline <- tryCatch(
      ps_cmdline(p),
      access_denied = function(e) NULL
    )
    if (!is.null(cmdline)) {
      exname <- basename(cmdline[1])
      if (str_starts_with(exname, n)) n <- exname
    }
  }
  n
}

#' @export

ps_exe <- function(p) {
  .Call(psll_exe, p)
}

#' @export

ps_cmdline <- function(p) {
  .Call(psll_cmdline, p)
}

#' @export

ps_status <- function(p) {
  .Call(psll_status, p)
}

#' @export

ps_username <- function(p) {
  .Call(psll_username, p)
}

#' @export

ps_create_time <- function(p) {
  .Call(psll_create_time, p)
}

#' @export

ps_cwd <- function(p) {
  .Call(psll_cwd, p)
}

#' @export

ps_uids <- function(p) {
  .Call(psll_uids, p)
}

#' @export

ps_gids <- function(p) {
  .Call(psll_gids, p)
}

#' @export

ps_terminal <- function(p) {
  ttynr <- .Call(psll_terminal, p)
  if (is.na(ttynr)) {
    NA_character_
  } else {
    tmap <- get_terminal_map()
    tmap[[as.character(ttynr)]]
  }
}

#' @export

ps_environ <- function(p) {
  parse_envs(.Call(psll_environ, p))
}

#' @export

ps_environ_raw <- function(p) {
  .Call(psll_environ, p)
}

#' @export

ps_num_threads <- function(p) {
  .Call(psll_num_threads, p)
}

#' @export

ps_cpu_times <- function(p) {
  .Call(psll_cpu_times, p)
}

#' @export

ps_memory_info <- function(p) {
  .Call(psll_memory_info, p)
}

#' @export

ps_send_signal <- function(p, sig) {
  .Call(psll_send_signal, p, sig)
}

#' @export

ps_suspend <- function(p) {
  .Call(psll_suspend, p)
}

#' @export

ps_resume <- function(p) {
  .Call(psll_resume, p)
}

#' @export

ps_terminate <- function(p) {
  .Call(psll_terminate, p)
}

#' @export

ps_kill <- function(p) {
  .Call(psll_kill, p)
}
