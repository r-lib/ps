
context("pid reuse")

test_that("pid reuse", {
  ## This is simulated, because it is quite some work to force a pid
  ## reuse on some systems. So we create a handle with the pid of a
  ## running process, but wrong (earlier) create time stamp.

  z <- processx::process$new(px(), c("sleep", "600"))
  on.exit(z$kill(), add = TRUE)
  zpid <- z$get_pid()

  ctime <- Sys.time() - 60
  attr(ctime, "tzone") <- "GMT"
  p <- ps_handle(zpid, ctime)

  expect_match(format(p), format_regexp())
  expect_output(print(p), format_regexp())

  expect_equal(ps_pid(p), zpid)
  expect_equal(ps_create_time(p), ctime)
  expect_false(ps_is_running(p))

  chk <- function(expr) {
    err <- tryCatch(expr, error = function(e) e)
    expect_s3_class(err, "no_such_process")
    expect_s3_class(err, "ps_error")
    expect_equal(err$pid, zpid)
  }

  ## All these error out with "no_such_process"
  chk(ps_status(p))
  chk(ps_ppid(p))
  chk(ps_parent(p))
  chk(ps_name(p))
  if (ps_os_type()[["POSIX"]]) chk(ps_uids(p))
  chk(ps_username(p))
  if (ps_os_type()[["POSIX"]]) chk(ps_gids(p))
  chk(ps_terminal(p))

  if (ps_os_type()[["POSIX"]]) chk(ps_send_signal(p, signals()$SIGINT))
  chk(ps_suspend(p))
  chk(ps_resume(p))
  if (ps_os_type()[["POSIX"]]) chk(ps_terminate(p))
  chk(ps_kill(p))

  chk(ps_exe(p))
  chk(ps_cmdline(p))
  chk(ps_environ(p))
  chk(ps_cwd(p))
  chk(ps_memory_info(p))
  chk(ps_cpu_times(p))
  chk(ps_num_threads(p))
  chk(ps_num_fds(p))
  chk(ps_open_files(p))
})
