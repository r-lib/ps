
context("system")

test_that("ps_pids", {
  pp <- ps_pids()
  expect_true(is.integer(pp))
  expect_true(Sys.getpid() %in% pp)
})

test_that("ps", {
  pp <- ps()
  expect_true(tibble::is_tibble(pp))
  expect_true(Sys.getpid() %in% pp$pid)
})

test_that("ps_boot_time", {
  bt <- ps_boot_time()
  expect_s3_class(bt, "POSIXct")
  expect_true(bt < Sys.time())
})
