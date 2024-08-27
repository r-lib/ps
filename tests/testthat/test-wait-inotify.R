test_that("dummy", {
  expect_true(TRUE)
})

if (ps_os_type()[["LINUX"]]) {
  fun <- function() {
    withr::local_envvar(PS_WAIT_FORCE_INOTIFY = "true")
    testthat::source_file(test_path("test-wait.R"), env = environment())
  }
  fun()
}
