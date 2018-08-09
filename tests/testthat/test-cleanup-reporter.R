
context("cleanup testthat reporter")

test_that("unit: test, mode: cleanup-fail", {

  out <- list()
  on.exit(if (!is.null(out$p)) out$p$kill(), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(proc_unit = "test"), {
        test_that("foobar", {
          out$p <<- processx::process$new(px(), c("sleep", "5"))
          out$running <<- out$p$is_alive()
        })
      }
    ),
    "did not clean up processes"
  )

  expect_true(out$running)
  deadline <- Sys.time() + 2
  while (out$p$is_alive() && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(Sys.time() < deadline)
  expect_false(out$p$is_alive())
})

test_that("unit: test, multiple processes", {

  out <- list()
  on.exit(if (!is.null(out$p1)) out$p1$kill(), add = TRUE)
  on.exit(if (!is.null(out$p2)) out$p2$kill(), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(proc_unit = "test"), {
        test_that("foobar", {
          out$p1 <<- processx::process$new(px(), c("sleep", "5"))
          out$p2 <<- processx::process$new(px(), c("sleep", "5"))
          out$running <<- out$p1$is_alive() && out$p2$is_alive()
        })
      }
    ),
    "px.*px"
  )

  expect_true(out$running)
  expect_false(out$p1$is_alive())
  expect_false(out$p2$is_alive())
})

test_that("on.exit() works", {

  out <- list()
  on.exit(if (!is.null(out$p)) out$p$kill(), add = TRUE)
  expect_success(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(proc_unit = "test"), {
        test_that("foobar", {
          out$p <<- processx::process$new(px(), c("sleep", "5"))
          on.exit(out$p$kill(), add = TRUE)
          out$running <<- out$p$is_alive()
        })
      }
    )
  )

  expect_true(out$running)
  expect_false(out$p$is_alive())
})

test_that("only report", {

  out <- list()
  on.exit(if (!is.null(out$p)) out$p$kill(), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(
        proc_unit = "test", proc_cleanup = FALSE, proc_fail = TRUE), {
          test_that("foobar", {
            out$p <<- processx::process$new(px(), c("sleep", "5"))
            out$running <<- out$p$is_alive()
          })
        }
    ),
    "did not clean up processes"
  )

  expect_true(out$running)
  expect_true(out$p$is_alive())
  out$p$kill()
})

test_that("only kill", {
  out <- list()
  on.exit(if (!is.null(out$p)) out$p$kill(), add = TRUE)
  with_reporter(
    CleanupReporter(testthat::SilentReporter)$new(
      proc_unit = "test", proc_cleanup = TRUE, proc_fail = FALSE), {
        test_that("foobar", {
          out$p <<- processx::process$new(px(), c("sleep", "5"))
          out$running <<- out$p$is_alive()
        })
        ## It must be killed by now
        test_that("foobar2", {
          deadline <- Sys.time() + 3
          while (out$p$is_alive()  && Sys.time() < deadline) Sys.sleep(0.05)
          out$running2 <<- out$p$is_alive()
        })
      }
  )

  expect_true(out$running)
  expect_false(out$running2)
  expect_false(out$p$is_alive())
})

test_that("unit: testsuite", {
  out <- list()
  on.exit(if (!is.null(out$p)) out$p$kill(), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(
        proc_unit = "testsuite", rconn_fail = FALSE, file_fail = FALSE), {
        test_that("foobar", {
          out$p <<- processx::process$new(px(), c("sleep", "5"))
          out$running <<- out$p$is_alive()
        })
        test_that("foobar2", {
          ## Still alive
          out$running2 <<- out$p$is_alive()
        })
      }
    ),
    "did not clean up processes"
  )

  expect_true(out$running)
  expect_true(out$running2)
  deadline <- Sys.time() + 3
  while (out$p$is_alive() && Sys.time() < deadline) Sys.sleep(0.05)
  expect_false(out$p$is_alive())
})

test_that("R connection cleanup, test, close, fail", {
  out <- list()
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  on.exit(try(close(out$conn), silent = TRUE), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(proc_fail = FALSE), {
        test_that("foobar", {
          out$conn <<- file(tmp, open = "w")
          out$open <<- isOpen(out$conn)
        })
      }
    ),
    "did not close R connections"
  )

  expect_true(out$open)
  expect_error(isOpen(out$conn))
})

test_that("R connection cleanup, test, do not close, fail", {
  out <- list()
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  on.exit(try(close(out$conn), silent = TRUE), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(
        proc_fail = FALSE, rconn_cleanup = FALSE), {
        test_that("foobar", {
          out$conn <<- file(tmp, open = "w")
          out$open <<- isOpen(out$conn)
        })
      }
    ),
    "did not close R connections"
  )

  expect_true(out$open)
  expect_true(isOpen(out$conn))
  expect_silent(close(out$conn))
})

test_that("R connection cleanup, test, close, do not fail", {
  out <- list()
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  on.exit(try(close(out$conn), silent = TRUE), add = TRUE)
  with_reporter(
    CleanupReporter(testthat::SilentReporter)$new(
      proc_fail = FALSE, rconn_fail = FALSE), {
        test_that("foobar", {
          out$conn <<- file(tmp, open = "w")
          out$open <<- isOpen(out$conn)
        })
      }
  )

  expect_true(out$open)
  expect_error(isOpen(out$conn))
})

test_that("R connections, unit: testsuite", {
  out <- list()
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  on.exit(try(close(out$conn), silent = TRUE), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(
        rconn_unit = "testsuite", proc_fail = FALSE, file_fail = FALSE), {
        test_that("foobar", {
          out$conn <<- file(tmp, open = "w")
          out$open <<- isOpen(out$conn)
        })
        test_that("foobar2", {
          ## Still alive
          out$open2 <<- isOpen(out$conn)
        })
      }
    ),
    "did not close R connections"
  )

  expect_true(out$open)
  expect_true(out$open2)
  expect_error(isOpen(out$conn))
})

test_that("connections already open are ignored", {

  tmp2 <- tempfile()
  on.exit(unlink(tmp2), add = TRUE)
  conn <- file(tmp2, open = "w")
  on.exit(try(close(conn), silent = TRUE), add = TRUE)

  out <- list()
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  on.exit(try(close(out$conn), silent = TRUE), add = TRUE)
  expect_success(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(proc_fail = FALSE), {
        test_that("foobar", {
          out$conn <<- file(tmp, open = "w")
          out$open <<- isOpen(out$conn)
          close(out$conn)
        })
      }
    )
  )

  expect_error(isOpen(out$conn))
  expect_true(isOpen(conn))
  expect_silent(close(conn))
})

test_that("File cleanup, test, fail", {
  out <- list()
  tmp <- tempfile()
  cat("data\ndata2\n", file = tmp)
  on.exit(unlink(tmp), add = TRUE)
  on.exit(try(close(out$conn), silent = TRUE), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(
        proc_fail = FALSE, rconn_cleanup = FALSE, rconn_fail = FALSE), {
        test_that("foobar", {
          out$conn <<- file(tmp, open = "r")
          out$open <<- isOpen(out$conn)
        })
      }
    ),
    "did not close open files"
  )

  expect_true(out$open)
  expect_true(isOpen(out$conn))
  close(out$conn)
})

test_that("File cleanup, unit: testsuite", {
  out <- list()
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  on.exit(try(close(out$conn), silent = TRUE), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(
        file_unit = "testsuite", proc_fail = FALSE, rconn_fail = FALSE,
        rconn_cleanup = FALSE), {
        test_that("foobar", {
          out$conn <<- file(tmp, open = "w")
          out$open <<- isOpen(out$conn)
        })
        test_that("foobar2", {
          ## Still alive
          out$open2 <<- isOpen(out$conn)
        })
      }
    ),
    "did not close open files"
  )

  expect_true(out$open)
  expect_true(out$open2)
  expect_true(isOpen(out$conn))
  close(out$conn)
})

test_that("files already open are ignored", {

  tmp2 <- tempfile()
  on.exit(unlink(tmp2), add = TRUE)
  conn <- file(tmp2, open = "w")
  on.exit(try(close(conn), silent = TRUE), add = TRUE)

  out <- list()
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  on.exit(try(close(out$conn), silent = TRUE), add = TRUE)
  expect_success(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(
        proc_fail = FALSE, rconn_fail = FALSE, rconn_cleanup = FALSE), {
        test_that("foobar", {
          out$conn <<- file(tmp, open = "w")
          out$open <<- isOpen(out$conn)
          close(out$conn)
        })
      }
    )
  )

  expect_error(isOpen(out$conn))
  expect_true(isOpen(conn))
  expect_silent(close(conn))
})
