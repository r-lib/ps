
map_chr <- function(.x, .f, ...) {
  vapply(X = .x, FUN = .f, FUN.VALUE = character(1), ...)
}

parse_envs <- function(x) {
  x <- enc2utf8(x)
  x <- strsplit(x, "=", fixed = TRUE)
  nms <- map_chr(x, "[[", 1)
  vls <- map_chr(x, function(x) paste(x[-1], collapse = "="))
  ord <- order(nms)
  structure(vls[ord], names = nms[ord], class = "Dlist")
}

## This is fully vectorized

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
