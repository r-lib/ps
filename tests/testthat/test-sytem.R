
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

test_that("ps_os_type", {
  os <- ps_os_type()
  expect_true(is.logical(os))
  expect_true(any(os))
  expect_equal(
    names(os),
    c("POSIX", "WINDOWS", "LINUX", "MACOS", "FREEBSD", "OPENBSD",
      "NETBSD", "BSD", "SUNOS", "AIX"))
})
