test_that("ps_fs_info", {
  skip_on_os("windows")

  # just test that it runs
  expect_silent(
    ps_fs_info(c("/", "~", "."))
  )
})