
context("system")

test_that("ps_pids", {
  pp <- ps_pids()
  expect_true(is.integer(pp))
  expect_true(Sys.getpid() %in% pp)
})
