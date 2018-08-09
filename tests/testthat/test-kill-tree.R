
context("kill-tree")

test_that("ps_mark_tree", {
  id <- ps_mark_tree()
  on.exit(Sys.unsetenv(id), add = TRUE)
  expect_true(is.character(id))
  expect_true(length(id) == 1)
  expect_false(is.na(id))
  expect_false(Sys.getenv(id) == "")
})

test_that("kill_tree",  {
  skip_on_cran()
  skip_in_rstudio()

  res <- ps_kill_tree(get_id())
  expect_equal(length(res), 0)
  expect_true(is.integer(res))

  ## Child processes
  id <- ps_mark_tree()
  on.exit(Sys.unsetenv(id), add = TRUE)
  p <- lapply(1:5, function(x) processx::process$new(px(), c("sleep", "10")))
  on.exit(lapply(p, function(x) x$kill()), add = TRUE)
  res <- ps_kill_tree(id)
  res <- res[names(res) %in% c("px", "px.exe")]
  expect_equal(length(res), 5)
  expect_equal(
    sort(as.integer(res)),
    sort(map_int(p, function(x) x$get_pid())))

  ## We need to wait a bit here, potentially, because the process
  ## might be a zombie, which is technically alive.
  now <- Sys.time()
  timeout <- now + 5
  while (any(map_lgl(p, function(pp) pp$is_alive())) &&
         Sys.time() < timeout) Sys.sleep(0.05)

  lapply(p, function(pp) expect_false(pp$is_alive()))
})

test_that("kill_tree, grandchild", {
  skip_on_cran()
  skip_in_rstudio()

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

  ## Older processx versions do not close the connections on kill,
  ## so the cleanup reporter picks them up
  lapply(p, function(pp) {
    close(pp$get_output_connection())
    close(pp$get_error_connection())
  })

  res <- res[names(res) %in% c("R", "Rterm.exe")]
  expect_equal(length(res), N * 2)
  expect_true(all(names(res) %in% c("R", "Rterm.exe")))
  cpids <- map_int(p, function(x) x$get_pid())
  expect_true(all(cpids %in% res))
  ccpids <- as.integer(dir(tmp))
  expect_true(all(ccpids %in% res)  )
})

test_that("kill_tree, orphaned grandchild", {
  skip_on_cran()
  skip_in_rstudio()

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
  res <- res[names(res) %in% c("px", "px.exe")]
  expect_equal(length(res), N)
  expect_true(all(names(res) %in% c("px", "px.exe")))
})

test_that("with_process_cleanup", {
  skip_on_cran()
  skip_in_rstudio()

  p <- NULL
  with_process_cleanup({
    p <- lapply(1:3, function(x) {
      processx::process$new(px(), c("sleep", "10"))
    })
    expect_equal(length(p), 3)
    lapply(p, function(pp) expect_true(pp$is_alive()))
  })

  expect_equal(length(p), 3)

  ## We need to wait a bit here, potentially, because the process
  ## might be a zombie, which is technically alive.
  now <- Sys.time()
  timeout <- now + 5
  while (any(map_lgl(p, function(pp) pp$is_alive())) &&
         Sys.time() < timeout) Sys.sleep(0.05)

  lapply(p, function(pp) expect_false(pp$is_alive()))
})

test_that("find_tree",  {
  skip_on_cran()
  skip_in_rstudio()
  skip_if_no_processx()

  res <- ps_find_tree(get_id())
  expect_equal(length(res), 0)
  expect_true(is.list(res))

  ## Child processes
  id <- ps_mark_tree()
  on.exit(Sys.unsetenv(id), add = TRUE)
  p <- lapply(1:5, function(x) processx::process$new(px(), c("sleep", "10")))
  on.exit(lapply(p, function(x) x$kill()), add = TRUE)
  res <- ps_find_tree(id)
  names <- not_null(lapply(res, function(p) fallback(ps_name(p), NULL)))
  res <- res[names %in% c("px", "px.exe")]
  expect_equal(length(res), 5)
  expect_equal(
    sort(map_int(res, ps_pid)),
    sort(map_int(p, function(x) x$get_pid())))

  lapply(p, function(x) x$kill())
})

test_that("find_tree, grandchild", {
  skip_on_cran()
  skip_in_rstudio()

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
  on.exit(ps_kill_tree(id), add = TRUE)

  timeout <- Sys.time() + 10
  while (length(dir(tmp)) < N && Sys.time() < timeout) Sys.sleep(0.1)

  res <- ps_find_tree(id)
  names <- not_null(lapply(res, function(p) fallback(ps_name(p), NULL)))
  res <- res[names %in% c("R", "Rterm.exe")]
  expect_equal(length(res), N * 2)
  cpids <- map_int(p, function(x) x$get_pid())
  res_pids <- map_int(res, ps_pid)
  expect_true(all(cpids %in% res_pids))
  ccpids <- as.integer(dir(tmp))
  expect_true(all(ccpids %in% res_pids))

  ## Older processx versions do not close the connections on kill,
  ## so the cleanup reporter picks them up
  lapply(p, function(pp) {
    pp$kill()
    close(pp$get_output_connection())
    close(pp$get_error_connection())
  })
})

test_that("find_tree, orphaned grandchild", {
  skip_on_cran()
  skip_in_rstudio()

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
  on.exit(ps_kill_tree(id), add = TRUE)

  timeout <- Sys.time() + 10
  while (sum(file_size(dir(tmp, full.names = TRUE)) > 0) < N &&
         Sys.time() < timeout) Sys.sleep(0.1)

  res <- ps_find_tree(id)
  names <- not_null(lapply(res, function(p) fallback(ps_name(p), NULL)))
  res <- res[names %in% c("px", "px.exe")]
  expect_equal(length(res), N)
})
