
context("process finished")

test_that("process already finished", {
  px <- processx::process$new(px(), c("sleep", "5"))
  on.exit(px$kill(), add = TRUE)
  pid <- px$get_pid()
  p <- ps_handle(pid)
  ct <- ps_create_time(p)

  px$kill()

  expect_false(px$is_alive())
  if (ps_os_type()[["POSIX"]]) {
    expect_equal(px$get_exit_status(), -9)
  }

  expect_match(format(p), format_regexp())
  expect_output(print(p), format_regexp())

  expect_equal(ps_pid(p), pid)
  if (has_processx()) expect_equal(ps_create_time(p), ct)
  expect_false(ps_is_running(p))

  chk <- function(expr) {
    err <- tryCatch(expr, error = function(e) e)
    expect_s3_class(err, "no_such_process")
    expect_s3_class(err, "ps_error")
    expect_equal(err$pid, pid)
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
  chk(ps_children(p))
  chk(ps_num_fds(p))
  chk(ps_open_files(p))
})
