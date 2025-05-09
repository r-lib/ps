# Named 'test-z-string' so it will run last.
# Testing for errors that crop up due to differences
# in start (load/attach) time for `ps`.

test_that("string", {
  ps <- ps_handle()

  # Values satisfy encoding assumptions
  expect_true(all(ps_pids() < 52^4))

  # Roundtrip through ps_string
  str <- expect_silent(ps_string(ps))
  ps2 <- expect_silent(ps_handle(str))

  # Got the same process back
  expect_true(ps_is_running(ps2))
  expect_identical(ps_pid(ps), ps_pid(ps2))
  expect_identical(ps_ppid(ps), ps_ppid(ps2))

  # Invalid process
  ps2 <- expect_silent(ps_handle(ps_pid(ps), ps_create_time(ps) + 1))
  expect_false(ps_is_running(ps2))
  str <- expect_silent(ps_string(ps2))
  ps2 <- expect_silent(ps_handle(str))
  expect_false(ps_is_running(ps2))

})


test_that("ipc string", {

  skip_on_cran()
  skip_on_covr()

  expect_true(
    callr::r(
      function(str) {
        ps <- ps::ps_handle(str)
        ps::ps_is_running(ps)
      },
      args = list(str = ps_string())
    )
  )

})
