
context("start")

test_that("ps_start", {
  args <- c("outln", "foo", "errln", "bar", "sleep", "10")
  x <- ps_start(px(), args, stdout = "|", stderr = "|")

  ## ps methods
  expect_equal(x$cmdline(), c(px(), args))
  expect_silent(x$cpu_times())
  expect_equal(normalizePath(x$cwd()), normalizePath(getwd()))
  expect_equal(x$environ()[["R_HOME"]], Sys.getenv("R_HOME"))
  expect_equal(x$exe(), realpath(px()))
  if (ps_os_type()[["POSIX"]]) expect_silent(x$uids())
  if (ps_os_type()[["POSIX"]]) expect_silent(x$gids())
  expect_true(x$is_running())
  expect_silent(x$memory_info())
  expect_true(x$name() %in% c("px", "px.exe"))
  expect_equal(x$num_threads(), 1)
  expect_equal(x$parent()$pid(), x$ppid())
  x$suspend()
  timeout <- Sys.time() + 60
  while (Sys.time() < timeout && x$status() != "stopped") Sys.sleep(0.05)
  expect_equal(x$status(), "stopped")
  x$resume()
  timeout <- Sys.time() + 60
  while (Sys.time() < timeout && x$status() == "stopped") Sys.sleep(0.05)
  expect_true(x$status() %in% c("running", "sleeping"))

  ## processx methods
  expect_equal(x$get_output_file(), "|")
  expect_equal(x$get_error_file(), "|")
  expect_s3_class(x$get_output_connection(), "processx_connection")
  expect_s3_class(x$get_error_connection(), "processx_connection")

  expect_equal(x$get_pid(), x$pid())
  expect_equal(x$get_start_time(), x$create_time())

  expect_true(x$has_error_connection())
  expect_false(x$has_input_connection())
  expect_true(x$has_output_connection())
  expect_false(x$has_poll_connection())

  expect_false(x$is_supervised())

  expect_true(x$is_incomplete_error())
  expect_true(x$is_incomplete_output())

  pr <- x$poll_io(-1)
  expect_true("ready" %in% pr)

  x$interrupt()
  x$wait(1000)

  expect_false(x$is_alive())

  expect_equal(x$read_all_output_lines(), "foo")
  expect_equal(x$read_all_error_lines(), "bar")
})
