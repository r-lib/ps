
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

test_that("disk_io", {
  # Validate inputs
  expect_error(ps_disk_io_counters(123), class = "invalid_argument")

  # Get total and perdisk results
  result <- ps_disk_io_counters()
  res_perdisk <- ps_disk_io_counters(perdisk=TRUE)

  # Check structure
  expect_named(
    result,
    c(
      "read_bytes",
      "write_bytes",
      "read_count",
      "write_count",
      "read_merged_count",
      "read_time",
      "write_merged_count",
      "write_time",
      "busy_time",
      "name"
    ),
    ignore.order=TRUE
  )
  expect_type(result, "list")
  expect_s3_class(result, "data.frame")

  # Non-Perdisk will be lte non-perdisk, due to virtual disks
  expect_lte(result$read_bytes, sum(res_perdisk$read_bytes))

  # Non perdisk returns 1 row
  expect_equal(nrow(res_perdisk), 1)
})
