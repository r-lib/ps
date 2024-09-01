test_that("ps_fs_info", {
  skip_on_os("windows")

  # just test that it runs
  expect_silent(
    ps_fs_info(c("/", "~", "."))
  )
})

test_that("disk_io", {
  result <- ps_disk_io_counters()

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
    ignore.order = TRUE
  )
  expect_type(result, "list")
  expect_s3_class(result, "data.frame")
})
