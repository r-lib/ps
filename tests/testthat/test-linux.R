
if (!ps_os_type()[["LINUX"]]) return()

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
  mem2 <- scan(sprintf("/proc/%d/statm", ps_pid(ps)), what = integer(),
               quiet = TRUE)
  page_size <- as.integer(system2("getconf", "PAGESIZE", stdout = TRUE))

  expect_equal(mem[["vms"]], mem2[[1]] * page_size)
  expect_equal(mem[["rss"]], mem2[[2]] * page_size)
})
