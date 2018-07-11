
context("kill-tree")

test_that("ps_mark_tree", {
  id <- ps_mark_tree()
  expect_true(is.character(id))
  expect_true(length(id) == 1)
  expect_false(is.na(id))
  expect_false(Sys.getenv(id) == "")
})

test_that("kill_tree",  {
  skip_on_cran()

  res <- ps_kill_tree(get_id())
  expect_equal(length(res), 0)
  expect_true(is.integer(res))

  ## Child processes
  id <- ps_mark_tree()
  on.exit(Sys.unsetenv(id), add = TRUE)
  p <- lapply(1:5, function(x) processx::process$new(px(), c("sleep", "10")))
  on.exit(lapply(p, function(x) x$kill()), add = TRUE)
  res <- ps_kill_tree(id)
  expect_equal(length(res), 5)
  expect_true(all(names(res) %in% c("px", "px.exe")))
  expect_equal(
    sort(as.integer(res)),
    sort(map_int(p, function(x) x$get_pid())))
  lapply(p, function(pp) expect_false(pp$is_alive()))
})

test_that("kill_tree, grandchild", {
  skip_on_cran()

  id <- ps_mark_tree()
  on.exit(Sys.unsetenv(id), add = TRUE)

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  N <- 3
  p <- lapply(1:N, function(x) {
    callr::r_bg(
      function(d) {
        callr::r(
          function(d) {
            cat("OK\n", file = file.path(d, Sys.getpid()))
            Sys.sleep(5)
          },
          args = list(d = d))
      },
      args = list(d = tmp))
  })
  on.exit(lapply(p, function(x) x$kill()), add = TRUE)

  timeout <- Sys.time() + 10
  while (length(dir(tmp)) < N && Sys.time() < timeout) Sys.sleep(0.1)

  res <- ps_kill_tree(id)
  expect_equal(length(res), N * 2)
  expect_true(all(names(res) %in% c("R", "RTerm.exe")))
  cpids <- map_int(p, function(x) x$get_pid())
  expect_true(all(cpids %in% res))
  ccpids <- as.integer(dir(tmp))
  expect_true(all(ccpids %in% res)  )
})

test_that("kill_tree, orphaned grandchild", {
  skip_on_cran()

  id <- ps_mark_tree()
  on.exit(Sys.unsetenv(id), add = TRUE)

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  cmdline <- paste(px(), "sleep 5")

  N <- 3
  lapply(1:N, function(x) {
    system2(px(), c("outln", "ok","sleep", "5"),
            stdout = file.path(tmp, x), wait = FALSE)
  })

  timeout <- Sys.time() + 10
  while (sum(file_size(dir(tmp, full.names = TRUE)) > 0) < N &&
         Sys.time() < timeout) Sys.sleep(0.1)

  res <- ps_kill_tree(id)
  expect_equal(length(res), N)
  expect_true(all(names(res) %in% c("px", "px.exe")))
})

test_that("with_process_cleanup", {
  skip_on_cran()

  p <- NULL
  with_process_cleanup({
    p <- lapply(1:5, function(x) {
      processx::process$new(px(), c("sleep", "10"))
    })
    expect_equal(length(p), 5)
    lapply(p, function(pp) expect_true(pp$is_alive()))
  })

  expect_equal(length(p), 5)
  lapply(p, function(pp) expect_false(pp$is_alive()))
})