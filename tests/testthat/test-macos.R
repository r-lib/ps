
if (!ps_os_type()[["MACOS"]]) return()

context("macos")

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
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_identical(ps_pid(ps), p1$get_pid())

  ## Even if it has quit already
  p2 <- processx::process$new("sleep", "10")
  pid2 <- p2$get_pid()
  on.exit(p2$kill(), add = TRUE)
  ps <-  ps_handle(pid2)
  p2$kill()

  expect_false(p2$is_alive())
  expect_identical(ps_pid(ps), pid2)
})

test_that("create_time", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_identical(p1$get_start_time(), ps_create_time(ps))

  ## Even if it has quit already
  p1$kill()
  expect_false(p1$is_alive())
  expect_identical(p1$get_start_time(), ps_create_time(ps))
})

test_that("is_running", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  p1$kill()
  expect_false(ps_is_running(ps))

  ## Zombie is running
  zpid <- zombie()
  on.exit(waitpid(zpid), add = TRUE)
  ps <- ps_handle(zpid)
  expect_true(ps_is_running(ps))
})

test_that("parent", {

})

test_that("ppid", {

})

test_that("name", {

})

test_that("exe", {

})

test_that("cmdline", {

})

test_that("status", {

})

test_that("username", {

})

test_that("cwd", {

})

test_that("uids", {

})

test_that("gids", {

})

test_that("terminal", {

})

test_that("environ", {

})

test_that("environ_raw", {

})

test_that("num_threads", {

})

test_that("cpu_times", {

})

test_that("memory_info", {

})

test_that("send_signal", {

})

test_that("suspend", {

})

test_that("resume", {

})

test_that("terminate", {

})

test_that("kill", {

})

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
})

test_that("process already finished", {
  px <- processx::process$new("sleep", "5")
  on.exit(px$kill(), add = TRUE)
  pid <- px$get_pid()
  p <- ps_handle(pid)
  px$kill()

  expect_false(px$is_alive())

  expect_match(format(p), format_regexp())
  expect_output(print(p), format_regexp())

  expect_equal(ps_pid(p), pid)
  expect_equal(ps_create_time(p), px$get_start_time())
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
  chk(ps_uids(p))
  chk(ps_username(p))
  chk(ps_gids(p))
  chk(ps_terminal(p))

  chk(ps_send_signal(p, signals()$SIGINT))
  chk(ps_suspend(p))
  chk(ps_resume(p))
  chk(ps_terminate(p))
  chk(ps_kill(p))

  chk(ps_exe(p))
  chk(ps_cmdline(p))
  chk(ps_environ(p))
  chk(ps_cwd(p))
  chk(ps_memory_info(p))
  chk(ps_cpu_times(p))
  chk(ps_num_threads(p))
})

test_that("pid reuse", {
  ## This is simulated, because it is quite some work to force a pid
  ## reuse on some systems. So we create a handle with the pid of a
  ## running process, but wrong (earlier) create time stamp.

  zpid <- zombie()
  on.exit(waitpid(zpid))
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
  chk(ps_uids(p))
  chk(ps_username(p))
  chk(ps_gids(p))
  chk(ps_terminal(p))

  chk(ps_send_signal(p, signals()$SIGINT))
  chk(ps_suspend(p))
  chk(ps_resume(p))
  chk(ps_terminate(p))
  chk(ps_kill(p))

  chk(ps_exe(p))
  chk(ps_cmdline(p))
  chk(ps_environ(p))
  chk(ps_cwd(p))
  chk(ps_memory_info(p))
  chk(ps_cpu_times(p))
  chk(ps_num_threads(p))
})
