
`%||%` <-  function(l, r) if (is.null(l)) r else l

not_null <- function(x) x[!map_lgl(x, is.null)]

map_chr <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = character(1), ...)
}

map_lgl <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = logical(1), ...)
}

map_int <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = integer(1), ...)
}

map_dbl <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = double(1), ...)
}

parse_envs <- function(x) {
  x <- enc2utf8(x)
  x <- strsplit(x, "=", fixed = TRUE)
  nms <- map_chr(x, "[[", 1)
  vls <- map_chr(x, function(x) paste(x[-1], collapse = "="))
  ord <- order(nms)
  structure(vls[ord], names = nms[ord], class = "Dlist")
}

## These two are fully vectorized

str_starts_with <- function(x, p) {
  ncp <- nchar(p)
  substr(x, 1, nchar(p)) == p
}

str_strip <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

r_version <- function(x) {
  v <- paste0(version[["major"]], ".", version[["minor"]])
  package_version(v)
}

file_size <- function(x) {
  if (r_version() >= "3.2.0") {
    file.info(x, extra_cols = FALSE)$size
  } else {
    file.info(x)$size
  }
}

format_unix_time <- function(z) {
  structure(z, class = c("POSIXct", "POSIXt"), tzone = "GMT")
}

NA_time <- function() {
  x <- Sys.time()
  x[] <- NA
  x
}

fallback <- function(expr, alternative) {
  tryCatch(
    expr,
    error = function(e) alternative
  )
}

read_lines <- function(path) {
  suppressWarnings(con <- file(path, open = "r"))
  on.exit(close(con), add = TRUE)
  suppressWarnings(readLines(con))
}

## We need to wait until the child becomes a zombie, otherwise
## it might still be in a running state

zombie <- function() {
  if (ps_os_type()[["POSIX"]]) {
    pid <- .Call(psp__zombie)
    ps <- ps_handle(pid)
    timeout <- Sys.time() + 5
    while (ps_status(ps) != "zombie" && Sys.time() < timeout)  {
      Sys.sleep(0.05)
    }
    if (ps_status(ps) == "zombie")  pid else stop("Cannot create zombie")
  }
}

waitpid <- function(pid) {
  if (ps_os_type()[["POSIX"]]) .Call(psp__waitpid, as.integer(pid))
}

## nocov start
caps <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

decorate_examples <- function(text, os = NULL) {
  text <- gsub("\n\n", "\n", text)

  if (!ps_is_supported()) {
    text <- paste0(
      "## ps does not support this platform, and cannot run the examples\n",
      "## Currently supported platforms are ", supported_str(), ".\n",
      ## "\\dontrun{",
      text
      ## "}"
    )

  } else if (!is.null(os) && !ps_os_type()[[os]]) {
    text <- paste0(
      "## these examples only work on ", caps(os), "\n",
      ## "\\dontrun{",
      text
      ##"}"
    )
  }

  ## paste0("\\examples{\n", text, "}")
  paste0("\\preformatted{", text, "}")
}
## nocov end

assert_string <- function(x) {
  if (is.character(x) && length(x) == 1 && !is.na(x)) return()
  stop(ps__invalid_argument(match.call()$x,
                            " is not a string (character scalar)"))
}

assert_character <- function(x) {
  if (is.character(x)) return()
  stop(ps__invalid_argument(match.call()$x,
                            " is not of type character"))
}

assert_pid <- function(x) {
  if (is.integer(x) && length(x) == 1 && !is.na(x)) return()
  stop(ps__invalid_argument(match.call()$x,
                            " is not a process id (integer scalar)"))
}

assert_time <- function(x) {
  if (inherits(x, "POSIXct")) return()
  stop(ps__invalid_argument(match.call()$x,
                            " must be a time stamp (POSIXt)"))
}

assert_ps_handle <- function(x) {
  if (inherits(x, "ps_handle")) return()
  stop(ps__invalid_argument(match.call()$x,
                            " must be a process handle (ps_handle)"))
}

assert_flag <- function(x) {
  if (is.logical(x) && length(x) == 1 && !is.na(x)) return()
  stop(ps__invalid_argument(match.call()$x,
                            " is not a flag (logical scalar)"))
}

assert_signal <- function(x) {
  if (is.integer(x) && length(x) == 1 && !is.na(x) &&
      x %in% unlist(signals())) return()
  stop(ps__invalid_argument(match.call()$x,
                            " is not a signal number (see ?signals())"))
}

realpath <- function(x) {
  if (ps_os_type()[["WINDOWS"]])
    .Call(psw__realpath, x)
  else
    normalizePath(x)
}

get_tool <- function(prog) {
  if (ps_os_type()[["WINDOWS"]]) prog <- paste0(prog, ".exe")
  exe <- system.file(package = "ps", "bin", .Platform$r_arch, prog)
  if (exe == "") {
    pkgpath <- system.file(package = "ps")
    if (basename(pkgpath) == "inst") pkgpath <- dirname(pkgpath)
    exe <- file.path(pkgpath, "src", prog)
    if (!file.exists(exe)) return("")
  }
  exe
}
