
format_regexp <- function() {
  "<ps::ps_handle> PID=[0-9]+, NAME=.*, AT="
}

parse_ps <- function(args) {
  out <- processx::run("ps", args)$stdout
  strsplit(out, "\n")[[1]][[2]]
}

parse_time <- function(x) {
  x <- utils::tail(c(0, 0, 0, as.numeric(strsplit(x, ":")[[1]])), 3)
  x[1] * 60 * 60 + x[2] * 60 + x[3]
}

wait_for_status <- function(ps, status, timeout = 5) {
  limit <- Sys.time() + timeout
  while (ps_status(ps) != status && Sys.time() < limit) Sys.sleep(0.05)
}
