
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

str_ends_with <- function(x, p) {
  ncx <- nchar(x)
  ncp <- nchar(p)
  substr(x, nchar(x) - ncp + 1L, ncx) == p
}

str_strip <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

readlink <- function(path) {
  assert_that(is_string(path))
  path <- .Call(ps__readlink, path)
  ## Certain paths have ' (deleted)' appended. Usually this is
  ## bogus as the file actually exists. Even if it doesn't we
  ## don't care.
  if (str_ends_with(path,  " (deleted)") && ! file.exists(path)) {
    path <- substr(path, 1, nchar(path) - 10)
  }
  path
}

raw_split <- function(x, byte) {
  if (!length(x)) return(list())
  sep <- which(x == byte)
  start <- c(1L, sep + 1L)
  end <- c(sep - 1L, length(x))
  lapply(seq_along(start), function(i) {
    if (start[i] > end[i]) raw(0) else x[start[i]:end[i]]
  })
}

file_size <- function(x) {
  file.info(x, extra_cols = FALSE)$size
}

## We cannot use file_size() here, because in /proc files have
## zero sizes

read_binary_file <- function(x) {
  ret <- raw()
  con <- file(x, open = "rb")
  on.exit(close(con), add = TRUE)
  while (length(new <- readBin(con, raw(), n = 1024))) {
    ret <- c(ret, new)
  }
  ret
}

path_is_absolute <- function(path) {
  if (ps_os_is_windows()) {
    path_is_absolute_win(path)
  } else {
    path_is_absolute_posix(path)
  }
}

path_is_absolute_win <- function(path) {
  p <- windows_path_split_drive(path)
  p$drive != "" | substr(p$path, 1L, 1L) %in% c("/", "\\")
}

windows_path_split_drive <- function(path) {
  npath <- gsub("/", "\\", path, fixed = TRUE)
  drive <- subpath <- character(length(path))
  na <- is.na(path)
  drive[na] <- subpath[na] <- NA_character_
  i <- !na & str_starts_with(npath, "\\\\") & substr(npath, 3, 3) != "\\"
  nunc <- ifelse(
    grepl("^\\\\\\\\[^\\\\]+\\\\[^\\\\]+.*$", npath[i]),
    nchar(sub("^(\\\\\\\\[^\\\\]+\\\\[^\\\\]+).*$", "\\1", npath[i])),
    0L)
  drive[i] <- substr(path[i], 1L, nunc)
  subpath[i] <- substr(path[i], nunc + 1L, nchar(path[i]))
  j <- !na & !i & substr(npath, 2L, 2L) == ":"
  drive[j] <- substr(path[j], 1L, 2L)
  subpath[j] <- substr(path[j], 3, nchar(path[j]))
  k <- !na & !i & !j
  subpath[k] <- path[k]
  list(drive = drive, path = subpath)
}

path_is_absolute_posix <- function(path) {
  str_starts_with(path, "/")
}

format_unix_time <- function(z) {
  as.POSIXct(z, origin = "1970-01-01", tz = "GMT")
}

NA_time <- function() {
  x <- Sys.time()
  x[] <- NA
  x
}

#' @importFrom prettyunits pretty_bytes

pretty_bytes_na <- function(x) {
  ifelse(is.na(x), NA_character_,  pretty_bytes(x))
}

fallback <- function(expr, alternative) {
  tryCatch(
    expr,
    error = function(e) alternative
  )
}
