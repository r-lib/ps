
format_regexp <- function() {
  "<ps::ps_handle> PID=[0-9]+, NAME=.*, AT="
}

parse_ps <- function(args) {
  out <- processx::run("ps", args)$stdout
  sub(" *$", "", strsplit(out, "\n")[[1]][[2]])
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

have_ipv6_support <- function() {
  ps_os_type()[["WINDOWS"]] ||
    !is.null(ps_env$constants$address_families$AF_INET6)
}

skip_without_ipv6 <- function() {
  if (!have_ipv6_support()) skip("Needs IPv6")
}

ipv6_url <- function() {
  paste0("https://", ipv6_host())
}

ipv6_host <- function() {
  "ipv6.test-ipv6.com"
}

have_ipv6_connection <- local({
  ok <- NULL
  myurl <- NULL
  function(url = ipv6_url()) {
    if (is.null(ok) || myurl != url) {
      myurl <<- url
      tryCatch({
        cx <- curl::curl(url)
        open(cx)
        ok <<- TRUE
      },
      error = function(x) ok <<- FALSE,
      finally = close(cx))
    }
    ok
  }
})

skip_without_ipv6_connection <- function() {
  if (!have_ipv6_connection()) skip("Needs working IPv6 connection")
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

## This is not perfect, e.g. we don't check that the numbers are <255,
## but will do for our purposes

is_ipv4_address <- function(x) {
  grepl("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$", x)
}

cleanup_process <- function(p) {
  tryCatch(close(p$get_input_connection()), error = function(x) x)
  tryCatch(close(p$get_output_connection()), error = function(x) x)
  tryCatch(close(p$get_error_connection()), error = function(x) x)
  tryCatch(close(p$get_poll_connection()), error = function(x) x)
  tryCatch(p$kill(), error = function(x) x)
  gc()
}

httpbin <- webfakes::new_app_process(
  webfakes::httpbin_app(),
  opts = webfakes::server_opts(num_threads = 6)
)
