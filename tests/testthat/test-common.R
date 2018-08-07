
context("common")

test_that("create self process", {
  expect_error(ps_handle("foobar"), class = "invalid_argument")
  expect_error(ps_handle(time = 123), class = "invalid_argument")

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
  ## Argument check
  expect_error(ps_pid(123), class = "invalid_argument")

  ## Self
  ps <- ps_handle()
  expect_identical(ps_pid(ps), Sys.getpid())

  ## Child
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_identical(ps_pid(ps), p1$get_pid())

  skip_if_no_processx()

  ## Even if it has quit already
  p2 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p2$kill(), add = TRUE)
  pid2 <- p2$get_pid()
  ps <- ps_handle(pid2)
  p2$kill()

  expect_false(p2$is_alive())
  expect_identical(ps_pid(ps), pid2)
})

test_that("create_time", {
  ## Argument check
  expect_error(ps_create_time(123), class = "invalid_argument")

  skip_if_no_processx()

  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_identical(p1$get_start_time(), ps_create_time(ps))
})

test_that("is_running", {
  ## Argument check
  expect_error(ps_is_running(123), class = "invalid_argument")

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
  ## Argument check
  expect_error(ps_parent(123), class = "invalid_argument")

  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  pp <- ps_parent(ps)
  expect_equal(ps_pid(pp), Sys.getpid())
})

test_that("ppid", {
  ## Argument check
  expect_error(ps_ppid(123), class = "invalid_argument")

  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_ppid(ps), Sys.getpid())
})

test_that("name", {
  ## Argument check
  expect_error(ps_name(123), class = "invalid_argument")

  skip_if_no_processx()

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
  ## Argument check
  expect_error(ps_exe(123), class = "invalid_argument")

  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_exe(ps), realpath(px()))
})

test_that("cmdline", {
  ## Argument check
  expect_error(ps_cmdline(123), class = "invalid_argument")

  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_cmdline(ps), c(px(), "sleep", "10"))
})

test_that("cwd", {
  ## Argument check
  expect_error(ps_cwd(123), class = "invalid_argument")

  p1 <- processx::process$new(px(), c("sleep", "10"), wd = tempdir())
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_cwd(ps), normalizePath(tempdir()))
})

test_that("environ, environ_raw", {
  ## Argument check
  expect_error(ps_environ(123), class = "invalid_argument")

  skip_if_no_processx()

  rnd <- basename(tempfile())
  p1 <- processx::process$new(px(), c("sleep", "10"), env = c(FOO = rnd))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_environ(ps)[["FOO"]], rnd)
  expect_true(paste0("FOO=", rnd) %in% ps_environ_raw(ps))
})

test_that("num_threads", {
  ## Argument check
  expect_error(ps_num_threads(123), class = "invalid_argument")

  ## sleep should be single-threaded
  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_num_threads(ps), 1)
  ## TODO: more threads?
})

test_that("suspend, resume", {
  ## Argument check
  expect_error(ps_suspend(123), class = "invalid_argument")
  expect_error(ps_resume(123), class = "invalid_argument")

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
  ## Argument check
  expect_error(ps_kill(123), class = "invalid_argument")

  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  ps_kill(ps)
  timeout <- Sys.time() + 5
  while (Sys.time() < timeout && ps_is_running(ps)) Sys.sleep(0.05)
  expect_false(p1$is_alive())
  expect_false(ps_is_running(ps))
  if (ps_os_type()[["POSIX"]]) {
    expect_equal(p1$get_exit_status(), - signals()$SIGKILL)
  }
})

test_that("children", {
  ## Argument check
  expect_error(ps_children(123), class = "invalid_argument")

  skip_if_no_processx()

  p1 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p1$kill(), add = TRUE)
  p2 <- processx::process$new(px(), c("sleep", "10"))
  on.exit(p2$kill(), add = TRUE)

  ch <- ps_children(ps_handle())
  expect_true(length(ch) >= 2)

  pids <- map_int(ch, ps_pid)
  expect_true(p1$get_pid() %in% pids)
  expect_true(p2$get_pid() %in% pids)

  ## We don't do this on Windows, because the parent process might be
  ## gone by now, and then it fails with no_such_process
  if (ps_os_type()[["POSIX"]]) {
    ch3 <- ps_children(ps_parent(ps_handle()), recursive = TRUE)
    pids3 <- map_int(ch3, ps_pid)
    expect_true(Sys.getpid() %in% pids3)
    expect_true(p1$get_pid() %in% pids3)
    expect_true(p2$get_pid() %in% pids3)
  }
})

test_that("num_fds", {
  skip_in_rstudio()
  skip_on_cran()

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  me <- ps_handle()
  orig <- ps_num_fds(me)

  f <- file(tmp, open = "w")
  on.exit(close(f), add = TRUE)

  expect_equal(ps_num_fds(me), orig + 1)
})

test_that("open_files", {
  skip_in_rstudio()
  skip_on_cran()

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  f <- file(tmp, open = "w")
  on.exit(try(close(f), silent = TRUE), add = TRUE)

  files <- ps_open_files(ps_handle())
  expect_true(basename(tmp) %in% basename(files$path))

  close(f)
  files <- ps_open_files(ps_handle())
  expect_false(basename(tmp) %in% basename(files$path))
})

test_that("interrupt", {
  skip_on_cran()
  px <- processx::process$new(px(), c("sleep", "10"))
  on.exit(px$kill(), add = TRUE)
  ps <- ps_handle(px$get_pid())

  expect_true(ps_is_running(ps))

  ps_interrupt(ps)

  deadline <- Sys.time() + 3
  while (ps_is_running(ps) && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(Sys.time() < deadline)
  expect_false(ps_is_running(ps))
  if (ps_os_type()[["POSIX"]]) expect_equal(px$get_exit_status(), -2)
})
