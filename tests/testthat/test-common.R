
context("common")

test_that("create self process", {
  ps <- ps_handle()
  expect_identical(ps_pid(ps), Sys.getpid())
})

test_that("format", {
  ps <- ps_handle()
  expect_match(format(ps), format_regexp())
})

test_that("print", {
  ps <- ps_handle()
  expect_output(print(ps), format_regexp())
})

test_that("pid", {
  ## Self
  ps <- ps_handle()
  expect_identical(ps_pid(ps), Sys.getpid())

  ## Child
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_identical(ps_pid(ps), p1$get_pid())

  ## Even if it has quit already
  p2 <- processx::process$new(px(), c("sleep", "10"))
  pid2 <- p2$get_pid()
  on.exit(p2$kill(), add = TRUE)
  ps <-  ps_handle(pid2)
  p2$kill()

  expect_false(p2$is_alive())
  expect_identical(ps_pid(ps), pid2)
})

test_that("create_time", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_identical(p1$get_start_time(), ps_create_time(ps))

  ## Even if it has quit already
  p1$kill()
  expect_false(p1$is_alive())
  expect_identical(p1$get_start_time(), ps_create_time(ps))
})

test_that("is_running", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  p1$kill()
  timeout <- Sys.time() + 5
  while (ps_is_running(ps) &&  Sys.time() < timeout) Sys.sleep(0.05)
  expect_false(ps_is_running(ps))
})

test_that("parent", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  pp <- ps_parent(ps)
  expect_equal(ps_pid(pp), Sys.getpid())
})

test_that("ppid", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_ppid(ps), Sys.getpid())
})

test_that("name", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_true(ps_name(ps) %in%  c("px", "px.exe"))

  ## Long names are not truncated
  file.copy(
    px(),
    tmp <- paste0(tempfile(pattern = "file1234567890123456"), ".bat"))
  on.exit(unlink(tmp), add = TRUE)
  Sys.chmod(tmp, "0755")

  p2 <- processx::process$new(tmp, c("sleep", "10"))
  on.exit(p2$kill(), add = TRUE)
  ps  <- ps_handle(p2$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_name(ps), basename(tmp))
})

test_that("exe", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_exe(ps), normalizePath(px()))
})

test_that("cmdline", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_cmdline(ps), c(px(), "sleep", "10"))
})

test_that("cwd", {
  p1 <- processx::process$new(px(), c("sleep", "10"), wd = tempdir())
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_cwd(ps), normalizePath(tempdir()))
})

test_that("environ, environ_raw", {
  rnd <- basename(tempfile())
  p1 <- processx::process$new(px(), c("sleep", "10"), env = c(FOO = rnd))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_environ(ps)[["FOO"]], rnd)
  expect_true(paste0("FOO=", rnd) %in% ps_environ_raw(ps))
})

test_that("num_threads", {
  ## sleep should be single-threaded
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_num_threads(ps), 1)
  ## TODO: more threads?
})

test_that("suspend, resume", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  ps_suspend(ps)
  timeout <- Sys.time() + 60
  while (Sys.time() < timeout && ps_status(ps) != "stopped") Sys.sleep(0.05)
  expect_equal(ps_status(ps), "stopped")
  expect_true(p1$is_alive())
  expect_true(ps_is_running(ps))

  ps_resume(ps)
  timeout <- Sys.time() + 60
  while (Sys.time() < timeout && ps_status(ps) == "stopped") Sys.sleep(0.05)
  expect_true(ps_status(ps) %in% c("running", "sleeping"))
  expect_true(p1$is_alive())
  expect_true(ps_is_running(ps))
  ps_kill(ps)
})

test_that("kill", {
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  ps_kill(ps)
  timeout <- Sys.time() + 60
  while (Sys.time() < timeout && ps_is_running(ps)) Sys.sleep(0.05)
  expect_false(p1$is_alive())
  expect_false(ps_is_running(ps))
  if (ps_os_type()[["POSIX"]]) {
    expect_equal(p1$get_exit_status(), - signals()$SIGKILL)
  }
})
