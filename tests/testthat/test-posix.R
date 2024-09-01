
if (!ps_os_type()[["POSIX"]]) return()

test_that("is_running", {
  ## Zombie is running
  zpid <- zombie()
  on.exit(waitpid(zpid), add = TRUE)
  ps <- ps_handle(zpid)
  expect_true(ps_is_running(ps))
})

test_that("terminal", {
  tty <- ps_terminal(ps_handle())
  if (is.na(tty)) skip("no terminal")
  expect_true(file.exists(tty))

  ## It is a character special file
  out <- processx::run("ls", c("-l", tty))$stdout
  expect_equal(substr(out, 1, 1), "c")
})

test_that("username, uids, gids", {
  if (Sys.which("ps") == "") skip("No ps program")
  ret <- system("ps -p 1 >/dev/null 2>/dev/null")
  if (ret != 0) skip("ps does not work properly")
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

test_that("kill with grace", {
  p1 <- processx::process$new(
    px(),
    c("sigterm", "ignore", "outln", "setup", "sleep", "3"),
    stdout = "|"
  )
  on.exit(p1$kill(), add = TRUE)
  ph1 <- p1$as_ps_handle()

  # need to wait until the SIGTERM handler is set up in px
  expect_equal(p1$poll_io(1000)[["output"]], "ready")
  expect_equal(ps_kill(ph1), "killed")
})

test_that("kill with grace, multiple processes", {
  # ignored SIGTERM completely
  p1 <- processx::process$new(
    px(),
    c("sigterm", "ignore", "outln", "setup", "sleep", "3"),
    stdout = "|"
  )
  on.exit(p1$kill(), add = TRUE)
  ph1 <- p1$as_ps_handle()

  # exits 0.5s later after SIGTERM
  p2 <- processx::process$new(
    px(),
    c("sigterm", "sleep", "0.5", "outln", "setup", "sleep", "3"),
    stdout = "|"
  )
  on.exit(p2$kill(), add = TRUE)
  ph2 <- p2$as_ps_handle()

  # exits on SIGTERM
  p3 <- processx::process$new(px(), c("sleep", "3"))
  on.exit(p3$kill(), add = TRUE)
  ph3 <- p3$as_ps_handle()

  # wait until signal handlers are set up
  expect_equal(p1$poll_io(1000)[["output"]], "ready")
  expect_equal(p2$poll_io(1000)[["output"]], "ready")
  expect_equal(
    ps_kill(list(ph1, ph2, ph3), grace = 1000),
    c("killed", "terminated", "terminated")
  )
})
