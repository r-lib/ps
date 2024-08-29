# https://github.com/r-lib/ps/issues/163
test_that("errors still cause a failure", {
  stop("oops")
})
