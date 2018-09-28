
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

px <- function() get_tool("px")

skip_in_rstudio <- function() {
  if (Sys.getenv("RSTUDIO") != "") skip("Cannot test in RStudio")
}

has_processx <- function() {
  requireNamespace("processx", quietly = TRUE) &&
    package_version(getNamespaceVersion("processx")) >= "3.1.0.9005"
}

skip_if_no_processx <- function() {
  if (!has_processx()) skip("Needs processx >= 3.1.0.9005 to run")
}

skip_without_program <- function(prog) {
  if (Sys.which(prog) == "") skip(paste(prog, "is not available"))
}

httpbin_url <- function() {
  "eu.httpbin.org"
}

is_offline <- (function() {
  offline <- NULL
  function() {
    if (is.null(offline)) {
      offline <<- tryCatch(
        is.na(pingr::ping_port(httpbin_url(), port = 443, count = 1L)),
        error = function(e) TRUE
      )
    }
    offline
  }
})()

skip_if_offline <- function() {
  if (is_offline()) skip("Offline")
}

wait_for_string <- function(proc, string, timeout) {
  deadline <- Sys.time() + as.difftime(timeout / 1000, units = "secs")
  str <- ""
  repeat {
    left <- max(as.double(deadline - Sys.time(), units = "secs"), 0)
    pr <- processx::poll(list(proc), as.integer(left * 1000))
    str <- paste(str, proc$read_error())
    if (grepl(string, str)) return()
    if (proc$has_output_connection()) read_output()
    if (deadline < Sys.time()) stop("Cannot start proces")
    if (!proc$is_alive()) stop("Cannot start process")
  }
}
