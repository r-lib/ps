
if (!ps_os_type()[["MACOS"]]) return()

context("macos")

test_that("status", {
  ## Argument check
  expect_error(ps_status(123), class = "invalid_argument")

  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_status(ps), "running")
  ps_suspend(ps)
  expect_equal(ps_status(ps), "stopped")
  ps_resume(ps)
  expect_equal(ps_status(ps), "running")
  ## TODO: can't easily test 'sleeping' and 'idle'
})

test_that("cpu_times", {
  skip_on_cran()

  ## Argument check
  expect_error(ps_cpu_times(123), class = "invalid_argument")

  p1 <- processx::process$new("ls", c("-lR", "/"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  Sys.sleep(0.2)
  ps_suspend(ps)
  ct <- ps_cpu_times(ps)
  ps2_user <- parse_time(parse_ps(c("-o", "utime", "-p", ps_pid(ps))))
  ps2_total <- parse_time(parse_ps(c("-o", "time", "-p", ps_pid(ps))))

  expect_true(abs(round(ct[["user"]], 2) - ps2_user) < 0.1)
  expect_true(abs(round(ct[["system"]], 2) - (ps2_total - ps2_user)) < 0.1)
})

test_that("memory_info", {
  skip_on_cran()

  ## Argument check
  expect_error(ps_memory_info(123), class = "invalid_argument")

  p1 <- processx::process$new("ls", c("-lR", "/"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  Sys.sleep(0.2)
  ps_suspend(ps)
  mem <- ps_memory_info(ps)
  ps2_rss <- as.numeric(parse_ps(c("-o", "rss", "-p", ps_pid(ps))))
  ps2_vms <- as.numeric(parse_ps(c("-o", "vsize", "-p", ps_pid(ps))))

  expect_equal(mem[["rss"]] / 1024, ps2_rss, tolerance = 10)
  expect_equal(mem[["vms"]] / 1024, ps2_vms, tolerance = 10)
})
