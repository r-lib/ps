
test_that("issue #129", {
  if (!ps_os_type()[["POSIX"]]) return()
  pss <- ps(user = "root", after = as.POSIXct('2022-05-15', tz = "GMT"))
  expect_s3_class(pss, "tbl")
})

test_that("can select columns", {
  skip_on_cran()
  expect_silent(ps(user = ps_username(), columns = c("pid", "username")))
  expect_silent(ps(user = ps_username(), columns = "*"))
})
