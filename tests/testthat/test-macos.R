
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
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  pp <- ps_parent(ps)
  expect_equal(ps_pid(pp), Sys.getpid())
})

test_that("ppid", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_ppid(ps), Sys.getpid())
})

test_that("name", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_name(ps), "sleep")

  ## Long names are not truncated
  file.copy(
    Sys.which("sleep"),
    tmp <- tempfile(pattern = "file1234567890123456"))
  on.exit(unlink(tmp), add = TRUE)
  Sys.chmod(tmp, "0755")

  p2 <- processx::process$new(tmp, "10")
  on.exit(p2$kill(), add = TRUE)
  ps  <- ps_handle(p2$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_name(ps), basename(tmp))
})

test_that("exe", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_exe(ps), Sys.which("sleep")[[1]])
})

test_that("cmdline", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_cmdline(ps), c("sleep", "10"))
})

test_that("status", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_status(ps), "running")
  ps_suspend(ps)
  expect_equal(ps_status(ps), "stopped")
  ps_resume(ps)
  expect_equal(ps_status(ps), "running")
  ## TODO: can't easily test 'sleeping' and 'idle'
})

test_that("username, uids, gids", {
  if (Sys.which("ps") == "") skip("No ps program")
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  ps2_username <- parse_ps(c("-o", "user", "-p", ps_pid(ps)))
  expect_equal(ps_username(ps), ps2_username)

  ps2_uid <- parse_ps(c("-o", "uid", "-p", ps_pid(ps)))
  expect_equal(ps_uids(ps)[["real"]], as.numeric(ps2_uid))

  ps2_gid <- parse_ps(c("-o", "rgid", "-p", ps_pid(ps)))
  expect_equal(ps_gids(ps)[["real"]], as.numeric(ps2_gid))
})

test_that("cwd", {
  p1 <- processx::process$new("sleep", "10", wd = tempdir())
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_cwd(ps), normalizePath(tempdir()))
})

test_that("terminal", {
  tty <- ps_terminal(ps_handle())
  if (is.na(tty)) skip("no terminal")
  expect_true(file.exists(tty))

  ## It is a character special file
  out <- processx::run("ls", c("-l", tty))$stdout
  expect_equal(substr(out, 1, 1), "c")
})

test_that("environ, environ_raw", {
  rnd <- basename(tempfile())
  p1 <- processx::process$new("sleep", "10", env = c(FOO = rnd))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))

  expect_equal(ps_environ(ps)[["FOO"]], rnd)
  expect_true(paste0("FOO=", rnd) %in% ps_environ_raw(ps))
})

test_that("num_threads", {
  ## sleep should be single-threaded
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())
  expect_true(ps_is_running(ps))
  expect_equal(ps_num_threads(ps), 1)
  ## TODO: more threads?
})

test_that("cpu_times", {
  skip_on_cran()
  p1 <- processx::process$new("ls", c("-lR", "/"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  Sys.sleep(0.2)
  ps_suspend(ps)
  ct <- ps_cpu_times(ps)
  ps2_user <- parse_time(parse_ps(c("-o", "utime", "-p", ps_pid(ps))))
  ps2_total <- parse_time(parse_ps(c("-o", "time", "-p", ps_pid(ps))))

  expect_true(abs(round(ct[["user"]], 2) - ps2_user) < 0.1)
  expect_true(abs(round(ct[["system"]], 2) - (ps2_total - ps2_user)) < 0.1)
})

test_that("memory_info", {
  skip_on_cran()
  p1 <- processx::process$new("ls", c("-lR", "/"))
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  Sys.sleep(0.2)
  ps_suspend(ps)
  mem <- ps_memory_info(ps)
  ps2_rss <- as.numeric(parse_ps(c("-o", "rss", "-p", ps_pid(ps))))
  ps2_vms <- as.numeric(parse_ps(c("-o", "vsize", "-p", ps_pid(ps))))

  expect_equal(mem[["rss"]] / 1024, ps2_rss)
  expect_equal(mem[["vms"]] / 1024, ps2_vms)
})

test_that("send_signal", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  ps_send_signal(ps, signals()$SIGINT)
  timeout <- Sys.time() + 60
  while (Sys.time() < timeout && p1$is_alive()) Sys.sleep(0.05)
  expect_false(p1$is_alive())
  expect_false(ps_is_running(ps))
  expect_equal(p1$get_exit_status(), - signals()$SIGINT)
})

test_that("suspend, resume", {
  p1 <- processx::process$new("sleep", "10")
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
  expect_equal(ps_status(ps), "running")
  expect_true(p1$is_alive())
  expect_true(ps_is_running(ps))
  ps_kill(ps)
})

test_that("terminate", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  ps_terminate(ps)
  timeout <- Sys.time() + 60
  while (Sys.time() < timeout && p1$is_alive()) Sys.sleep(0.05)
  expect_false(p1$is_alive())
  expect_false(ps_is_running(ps))
  expect_equal(p1$get_exit_status(), - signals()$SIGTERM)
})

test_that("kill", {
  p1 <- processx::process$new("sleep", "10")
  on.exit(p1$kill(), add = TRUE)
  ps <- ps_handle(p1$get_pid())

  ps_kill(ps)
  timeout <- Sys.time() + 60
  while (Sys.time() < timeout && p1$is_alive()) Sys.sleep(0.05)
  expect_false(p1$is_alive())
  expect_false(ps_is_running(ps))
  expect_equal(p1$get_exit_status(), - signals()$SIGKILL)
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
