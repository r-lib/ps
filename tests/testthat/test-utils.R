
context("utils")

test_that("errno", {
  err <- errno()
  expect_true(is.list(err))

  if (ps_os_type()[["POSIX"]]) {
    expect_true("EINVAL" %in% names(err))
    expect_true("EBADF" %in% names(err))
  }
})

test_that("str_strip", {
  tcs <- list(
    list("",  ""),
    list(" ", ""),
    list("a ", "a"),
    list(" a", "a"),
    list(" a ", "a"),
    list("    a       ", "a"),
    list(character(), character()),
    list(c("", NA, "a "), c("", NA, "a")),
    list("\ta\n", "a")
  )

  for (tc in tcs) {
    expect_identical(str_strip(tc[[1]]), tc[[2]])
  }
})

test_that("NA_time", {
  nat <- NA_time()
  expect_s3_class(nat, "POSIXct")
  expect_true(length(nat) == 1 && is.na(nat))
})

test_that("read_lines", {
  tmp <- tempfile()
  cat("foo\nbar\nfoobar", file = tmp)
  expect_silent(l <- read_lines(tmp))
  expect_equal(l, c("foo", "bar", "foobar"))
})
