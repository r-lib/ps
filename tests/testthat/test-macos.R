
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

  ## These raise zombie_process errors
  expect_error(ps_exe(p), class = "zombie_process")
  expect_error(ps_cmdline(p), class = "zombie_process")
  expect_error(ps_environ(p), class = "zombie_process")
  expect_error(ps_cwd(p), class = "zombie_process")
  expect_error(ps_memory_info(p), class = "zombie_process")
  expect_error(ps_cpu_times(p), class = "zombie_process")
  expect_error(ps_num_threads(p), class = "zombie_process")
})

## TODO: test pid reuse

test_that("process already finished", {
  px <- processx::process$new("sleep", "5")
  on.exit(px$kill(), add = TRUE)
  p <- ps_handle(px$get_pid())
  px$kill()

  expect_false(px$is_alive())

  expect_match(format(p), format_regexp())
  expect_output(print(p), format_regexp())

  expect_equal(ps_pid(p), px$get_pid())
  expect_equal(ps_create_time(p), px$get_start_time())
  expect_false(ps_is_running(p))

  ## All these error out with "no_such_process"
  expect_error(ps_status(p), class = "no_such_process")
  expect_error(ps_ppid(p), class = "no_such_process")
  expect_error(ps_parent(p), class = "no_such_process")
  expect_error(ps_name(p), class = "no_such_process")
  expect_error(ps_uids(p), class = "no_such_process")
  expect_error(ps_username(p), class = "no_such_process")
  expect_error(ps_gids(p), class = "no_such_process")
  expect_error(ps_terminal(p), class = "no_such_process")

  expect_error(ps_send_signal(p, signals()$SIGINT),
               class = "no_such_process")
  expect_error(ps_suspend(p), class = "no_such_process")
  expect_error(ps_resume(p), class = "no_such_process")
  expect_error(ps_terminate(p), class = "no_such_process")
  expect_error(ps_kill(p), class = "no_such_process")

  expect_error(ps_exe(p), class = "no_such_process")
  expect_error(ps_cmdline(p), class = "no_such_process")
  expect_error(ps_environ(p), class = "no_such_process")
  expect_error(ps_cwd(p), class = "no_such_process")
  expect_error(ps_memory_info(p), class = "no_such_process")
  expect_error(ps_cpu_times(p), class = "no_such_process")
  expect_error(ps_num_threads(p), class = "no_such_process")
})
