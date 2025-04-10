
#' Encode a `ps_handle` as a short string
#'
#' A convenient format for passing between processes, naming semaphores, or
#' using as a directory/file name. Will always be 14 alphanumeric characters,
#' with the first and last characters guarantied to be letters. Encodes the
#' pid and creation time for a process.
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

ps_string <- function (p = ps_handle()) {
  assert_ps_handle(p)
  ps__str_encode(ps_pid(p), ps_create_time(p))
}


ps__str_encode <- function (process_id, time) {

  whole_secs <- as.integer(time)
  micro_secs <- as.numeric(time) %% 1 * 1000000

  # Assumptions:
  #   time between Jan 1st 1970 and Dec 5th 3769.
  #   max time precision = 1/1,000,000 of a second.
  #   pid <= 7,311,615 (current std max = 4,194,304).

  map <- c(letters, LETTERS, 0:9)

  paste(collapse = '', map[1 + c(
    floor(process_id / 52 ^ (0:3)) %% 52,
    floor(whole_secs / 62 ^ (0:5)) %% 62,
    floor(micro_secs / 52 ^ (0:3)) %% 52 )])
}


ps__str_decode <- function (str) {

  map <- structure(0:61, names = c(letters, LETTERS, 0:9))
  val <- map[strsplit(str, '', fixed = TRUE)[[1]]]

  process_id <- sum(val[01:04] * 52 ^ (0:3))
  whole_secs <- sum(val[05:10] * 62 ^ (0:5))
  micro_secs <- sum(val[11:14] * 52 ^ (0:3))

  time <- whole_secs + (micro_secs / 1000000)
  time <- as.POSIXct(time, tz = 'GMT', origin = '1970-01-01')

  p <- ps_handle(pid = process_id)

  # No matching process with pid and time +/- 2 microseconds
  if (!ps_is_running(p) || abs(ps_create_time(p) - time) > 2/1000000)
    p <- ps_handle(pid = process_id, time = time)

  return(p)
}
