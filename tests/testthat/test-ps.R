
test_that("issue #129", {
  if (!ps_os_type()[["POSIX"]]) return()
  pss <- ps(user = "root", after = as.POSIXct('2022-05-15', tz = "GMT"))
  expect_s3_class(pss, "tbl")
})
