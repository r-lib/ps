
context("cleanup testthat reporter")

test_that("unit: test, mode: cleanup-fail", {

  out <- list()
  on.exit(if (!is.null(out$p)) out$p$kill(), add = TRUE)
  expect_failure(
    with_reporter(
      CleanupReporter(testthat::SilentReporter)$new(unit = "test"), {
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
      CleanupReporter(testthat::SilentReporter)$new(unit = "test"), {
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
      CleanupReporter(testthat::SilentReporter)$new(unit = "test"), {
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
        unit = "test", process_cleanup = FALSE, process_fail = TRUE), {
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
      unit = "test", process_cleanup = TRUE, process_fail = FALSE), {
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
      CleanupReporter(testthat::SilentReporter)$new(unit = "testsuite"), {
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
