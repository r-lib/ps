
if (!ps_os_type()[["WINDOWS"]]) return()

context("windows")

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
