
if (!ps_os_type()[["POSIX"]]) return()

context("posix-zombie")

test_that("zombie api", {
  zpid <- zombie()
  on.exit(waitpid(zpid), add = TRUE)
  p <- ps_handle(zpid)
  me <- ps_handle()

  expect_match(format(p), format_regexp())
  expect_output(print(p), format_regexp())

  expect_equal(ps_pid(p), zpid)
  expect_true(ps_create_time(p) > ps_create_time(me))
  expect_true(ps_is_running(p))
  expect_equal(ps_status(p), "zombie")
  expect_equal(ps_ppid(p), Sys.getpid())
  expect_equal(ps_pid(ps_parent(p)), Sys.getpid())
  expect_equal(ps_name(p), ps_name(me))
  expect_identical(ps_uids(p), ps_uids(me))
  expect_identical(ps_username(p), ps_username(me))
  expect_identical(ps_gids(p), ps_gids(me))
  expect_identical(ps_terminal(p), ps_terminal(me))
  expect_silent(ps_children(p))

  ## You can still send signals if you like
  expect_silent(ps_send_signal(p, signals()$SIGINT))
  expect_equal(ps_status(p), "zombie")
  expect_silent(ps_suspend(p))
  expect_equal(ps_status(p), "zombie")
  expect_silent(ps_resume(p))
  expect_equal(ps_status(p), "zombie")
  expect_silent(ps_terminate(p))
  expect_equal(ps_status(p), "zombie")
  expect_silent(ps_kill(p))
  expect_equal(ps_status(p), "zombie")

  chk <- function(expr) {
    err <- tryCatch(expr, error = function(e) e)
    expect_s3_class(err, "zombie_process")
    expect_s3_class(err, "ps_error")
    expect_equal(err$pid, zpid)
  }

  ## These raise zombie_process errors
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
