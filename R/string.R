#' Encode a `ps_handle` as a short string
#'
#' A convenient format for passing between processes, naming semaphores, or
#' using as a directory/file name. Will always be 11 alphanumeric characters,
#' with the first character guarantied to be a letter. Encodes the pid and
#' creation time for a process.
#'
#' @param p Process handle.
#'
#' @return A process string (scalar character), that can be passed to
#' `ps_handle()` in place of a pid.
#'
#' @export
#' @examplesIf ps::ps_is_supported() && ! ps:::is_cran_check()
#' (p <- ps_handle())
#' (str <- ps_string(p))
#' ps_handle(pid = str)

ps_string <- function(p = ps_handle()) {
  assert_ps_handle(p)
  ps__str_encode(p)
}


ps__str_encode <- function(p) {

  process_id  <- ps_pid(p)
  create_secs <- as.numeric(ps_create_time(p))
  boot_secs   <- as.numeric(ps_boot_time())
  offset_ms   <- floor((create_secs - boot_secs) * 1000)

  # Assumptions:
  #   System uptime < 111 years.
  #   PIDs are not reused within the same millisecond.
  #   PID <= 7,311,615 (current std max = 4,194,304).

  map <- c(letters, LETTERS, 0:9)

  paste(
    collapse = '',
    map[
      1 +
        c(
          floor(process_id / 52^(3:0)) %% 52,
          floor(offset_ms  / 62^(6:0)) %% 62
        )
    ]
  )
}


ps__str_decode <- function(str) {
  map <- structure(0:61, names = c(letters, LETTERS, 0:9))
  val <- map[strsplit(str, '', fixed = TRUE)[[1]]]

  process_id  <- sum(val[01:04] * 52^(3:0))
  offset_ms   <- sum(val[05:11] * 62^(6:0))
  create_time <- ps_boot_time() + (offset_ms / 1000)

  # Allow fuzzy-matching the microseconds
  tryCatch(
    expr = {
      p <- ps_handle(pid = process_id)
      stopifnot(abs(ps_create_time(p) - create_time) < 1 / 1000)
      p
    },
    error = function(e) {
      ps_handle(pid = process_id, time = create_time)
    }
  )
}
