
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

  x <- ps_start(px(), c("sleep", "5"))
  on.exit(x$kill(), add = TRUE)
  pp <- ps(after = Sys.time() - 60 * 60)
  ct <- lapply(pp$pid, function(p) {
    tryCatch(ps_create_time(ps_handle(p)), error = function(e) NULL)
  })
  ct <- not_null(ct)
  expect_true(all(map_lgl(ct, function(x) x > Sys.time() - 60 * 60)))

  pp <- ps(user = ps_username(ps_handle()))
  expect_true(all(pp$username == ps_username(ps_handle())))
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
    c("POSIX", "WINDOWS", "LINUX", "MACOS"))
})

test_that("ps_is_supported", {
  expect_equal(any(ps_os_type()), ps_is_supported())
})

test_that("supported_str", {
  expect_equal(supported_str(), "Windows, Linux, Macos")
})

test_that("ps_os_name", {
  expect_true(ps_os_name() %in% names(ps_os_type()))
})
