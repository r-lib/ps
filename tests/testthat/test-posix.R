
if (!ps_os_type()[["POSIX"]]) return()

context("posix")

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
  gc()
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
  gc()
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
