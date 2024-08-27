test_that("single process", {
  skip_on_cran()
  p <- processx::process$new(px(), c("sleep", "600"))
  on.exit(p$kill(), add = TRUE)
  ph <- ps_handle(p$get_pid())

  expect_false(ps_wait(ph, 0))
  expect_false(ps_wait(list(ph), 0))

  tic <- Sys.time()
  expect_false(ps_wait(ph, 100))
  toc <- Sys.time()
  expect_true(toc - tic >= as.difftime(0.1, units = "secs"))

  p$kill()
  tic <- Sys.time()
  expect_true(ps_wait(ph, 1000))
  toc <- Sys.time()
  expect_true(toc - tic < as.difftime(1, units = "secs"))
})

test_that("multiple processes", {
  skip_on_cran()
  p1 <- processx::process$new(px(), c("sleep", "600"))
  on.exit(p1$kill(), add = TRUE)
  ph1 <- ps_handle(p1$get_pid())
  p2 <- processx::process$new(px(), c("sleep", "600"))
  on.exit(p2$kill(), add = TRUE)
  ph2 <- ps_handle(p2$get_pid())
  p3 <- processx::process$new(px(), c("sleep", "600"))
  on.exit(p3$kill(), add = TRUE)
  ph3 <- ps_handle(p3$get_pid())

  expect_equal(ps_wait(list(ph1, ph2, ph3), 0), c(FALSE, FALSE, FALSE))
  expect_equal(ps_wait(list(ph1, ph2, ph3), 100), c(FALSE, FALSE, FALSE))

  p1$kill()
  p2$kill()
  p3$kill()
  tic <- Sys.time()
  expect_equal(ps_wait(list(ph1, ph2, ph3), 1000), c(TRUE, TRUE, TRUE))
  toc <- Sys.time()
  expect_true(toc - tic < as.difftime(1, units = "secs"))
})

test_that("stress test", {
  skip_on_cran()
  pp <- lapply(1:100, function(i) {
    processx::process$new(px(), c("sleep", "2"))
  })
  on.exit(lapply(pp, function(p) p$kill()), add = TRUE)
  pps <- lapply(pp, function(p) ps_handle(p$get_pid()))

  tic <- Sys.time()
  ret <- ps_wait(pps, 0)
  toc <- Sys.time()
  expect_equal(ret, rep(FALSE, length(pp)))
  expect_true(toc - tic < as.difftime(0.5, units = "secs"))

  tic <- Sys.time()
  ret <- ps_wait(pps, 3000)
  toc <- Sys.time()
  expect_equal(ret, rep(TRUE, length(pp)))
  expect_true(toc - tic < as.difftime(3, units = "secs"))
})
