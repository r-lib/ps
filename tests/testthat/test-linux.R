if (!ps_os_type()[["LINUX"]]) {
  return()
}

test_that("status", {
  ## Argument check
  expect_error(ps_status(123), class = "invalid_argument")

  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  wait_for_status(ps, "sleeping")
  expect_equal(ps_status(ps), "sleeping")
  ps_suspend(ps)
  wait_for_status(ps, "stopped")
  expect_equal(ps_status(ps), "stopped")
  ps_resume(ps)
  wait_for_status(ps, "sleeping")
  expect_equal(ps_status(ps), "sleeping")
  ## TODO: rest?
})

## TODO: cpu_times ??? We apparently cannot get them from ps

## Helper: read starttime (ticks since boot) from /proc/<pid>/stat,
## handling process names that contain spaces or parentheses.
proc_starttime_ticks <- function(pid) {
  raw <- readLines(sprintf("/proc/%d/stat", pid))
  ## Everything after the last ')' is the fixed-format tail
  after_paren <- sub("^.*\\) ", "", raw)
  fields <- strsplit(after_paren, " ")[[1]]
  ## starttime is the 20th field after the name (field 22 overall)
  as.numeric(fields[20])
}

## Helper: integer boot time from /proc/stat btime (what old ps used)
proc_stat_btime <- function() {
  line <- grep("^btime ", readLines("/proc/stat"), value = TRUE)
  as.numeric(strsplit(line, " +")[[1]][2])
}

test_that("ps_handle validates legacy (integer /proc/stat btime) create_time", {
  skip_if_no_processx()

  p <- processx::process$new("sleep", "100")
  on.exit(p$kill(), add = TRUE)
  pid <- p$get_pid()

  ## Build the create_time the way old ps did: integer boot time + ticks/CLK_TCK
  btime   <- proc_stat_btime()
  ticks   <- proc_starttime_ticks(pid)
  clk_tck <- as.integer(system2("getconf", "CLK_TCK", stdout = TRUE))
  legacy_ct <- structure(
    btime + ticks / clk_tck,
    class = c("POSIXct", "POSIXt"),
    tzone = "GMT"
  )

  h <- ps_handle(pid, legacy_ct)
  expect_true(ps_is_running(h))
  expect_equal(ps_name(h), "sleep")
  ps_suspend(h)
  wait_for_status(h, "stopped")
  expect_equal(ps_status(h), "stopped")
  ps_resume(h)
  wait_for_status(h, "sleeping")
  expect_equal(ps_status(h), "sleeping")
})

test_that("ps_handle validates precise (CLOCK_REALTIME-CLOCK_MONOTONIC) create_time", {
  skip_if_no_processx()

  p <- processx::process$new("sleep", "100")
  on.exit(p$kill(), add = TRUE)
  pid <- p$get_pid()

  ## ps_handle(pid) auto-discovers the precise create_time internally
  precise_ct <- ps_create_time(ps_handle(pid))

  ## Re-create the handle with that explicit precise time
  h <- ps_handle(pid, precise_ct)
  expect_true(ps_is_running(h))
  expect_equal(ps_name(h), "sleep")
  ps_suspend(h)
  wait_for_status(h, "stopped")
  expect_equal(ps_status(h), "stopped")
  ps_resume(h)
  wait_for_status(h, "sleeping")
  expect_equal(ps_status(h), "sleeping")
})

test_that("memory_info", {
  ## Argument check
  expect_error(ps_memory_info(123), class = "invalid_argument")

  skip_on_cran()

  p1 <- processx::process$new("ls", c("-lR", "/"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  Sys.sleep(0.2)
  ps_suspend(ps)
  mem <- ps_memory_info(ps)
  mem2 <- scan(
    sprintf("/proc/%d/statm", ps_pid(ps)),
    what = integer(),
    quiet = TRUE
  )
  page_size <- as.integer(system2("getconf", "PAGESIZE", stdout = TRUE))

  expect_equal(mem[["vms"]], mem2[[1]] * page_size)
  expect_equal(mem[["rss"]], mem2[[2]] * page_size)
})
