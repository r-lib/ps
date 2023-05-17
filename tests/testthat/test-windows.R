
if (!ps_os_type()[["WINDOWS"]]) return()

test_that("uids, gids", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  err <- tryCatch(ps_uids(ps), error = function(e) e)
  expect_s3_class(err, "not_implemented")
  expect_s3_class(err, "ps_error")
  err <- tryCatch(ps_gids(ps), error = function(e) e)
  expect_s3_class(err, "not_implemented")
  expect_s3_class(err, "ps_error")
})

test_that("terminal", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_identical(ps_terminal(ps), NA_character_)
})

## TODO: username
## TODO: cpu_times
## TODO: memory_info

test_that("total and available mem", {
  l <- .Call(ps__system_memory)[c("total", "avail")]
  expect_true(is.numeric(l$total))
  expect_true(is.numeric(l$avail))
  expect_lte(l$avail, l$total)
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
  expect_equal(nrow(result), 1)
})

