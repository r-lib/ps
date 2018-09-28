
context("connections")

test_that("empty set", {
  px <- processx::process$new(
    px(), c("sleep", "5"),
    poll_connection = FALSE)
  on.exit(px$kill(), add = TRUE)
  pid <- px$get_pid()
  p <- ps_handle(pid)

  cl <- ps_connections(p)
  expect_equal(nrow(cl), 0)
  expect_s3_class(cl, "data.frame")
  expect_s3_class(cl, "tbl_df")
  expect_equal(
    names(cl),
    c("fd", "family", "type", "laddr", "lport", "raddr", "rport", "state"))
})

test_that("UNIX sockets", {
  if (!ps_os_type()[["POSIX"]]) skip("No UNIX sockets")

  px <- processx::process$new(px(), c("sleep", "5"), stdout = "|")
  on.exit(px$kill(), add = TRUE)
  pid <- px$get_pid()
  p <- ps_handle(pid)

  cl <- ps_connections(p)
  expect_equal(nrow(cl), 1)
  expect_s3_class(cl, "data.frame")
  expect_s3_class(cl, "tbl_df")
  expect_equal(cl$fd, 1)
  expect_equal(cl$family, "AF_UNIX")
  expect_equal(cl$type, "SOCK_STREAM")
  expect_identical(cl$laddr, NA_character_)
  expect_identical(cl$lport, NA_integer_)
  expect_identical(cl$raddr, NA_character_)
  expect_identical(cl$lport, NA_integer_)
  expect_identical(cl$state, NA_character_)
})

test_that("UNIX sockets with path", {
  if (!ps_os_type()[["POSIX"]]) skip("No UNIX sockets")
  skip_without_program("socat")
  skip_if_no_processx()

  sfile <- tempfile()
  sfile <- file.path(normalizePath(dirname(sfile)), basename(sfile))
  on.exit(unlink(sfile, recursive = TRUE), add = TRUE)
  nc <- processx::process$new(
    "socat", c("-", paste0("UNIX-LISTEN:", sfile)))
  on.exit(nc$kill(), add = TRUE)
  p <- nc$as_ps_handle()

  ## Might need to wait for socat to start listening on the socket
  deadline <- Sys.time() + as.difftime(5, units = "secs")
  while (nc$is_alive() && !file.exists(sfile) && Sys.time() < deadline) {
    Sys.sleep(0.1)
  }

  cl <- ps_connections(p)
  cl <- cl[!is.na(cl$laddr) & cl$laddr == sfile, ]
  expect_equal(nrow(cl), 1)
})

test_that("TCP", {
  skip_if_offline()
  before <- ps_connections(ps_handle())
  ips <- curl::nslookup(httpbin_url(), multiple = TRUE, ipv4_only = TRUE)
  cx <- curl::curl(httpbin_url(), open = "r")
  on.exit(close(cx), add = TRUE)
  after <- ps_connections(ps_handle())
  new <- after[! after$fd %in% before$fd, ]
  expect_true(new$raddr %in% ips)
})

test_that("TCP on loopback", {
  skip_without_program("socat")
  skip_if_no_processx()

  nc <- processx::process$new(
    "socat", c("-d", "-d", "-ls", "-", "TCP4-LISTEN:0"), stderr = "|")
  on.exit(nc$kill(), add = TRUE)
  p <- nc$as_ps_handle()

  wait_for_string(nc, "listening on", timeout = 2000)

  cl <- ps_connections(p)
  cl <- cl[!is.na(cl$state) & cl$state == "CONN_LISTEN", ]
  expect_equal(nrow(cl), 1)
  expect_true(cl$state == "CONN_LISTEN")
  port <- cl$lport

  nc2 <- processx::process$new(
    "socat", c("-", paste0("TCP4-CONNECT:127.0.0.1:", port)), stdin = "|")
  on.exit(nc2$kill(), add = TRUE)
  p2 <- nc2$as_ps_handle()

  p2
})

test_that("UDP", {
  skip_if_offline()
})

test_that("UDP on loopback", {
  skip_without_program("socat")
  skip_if_no_processx()

})

test_that("TCP6 on loopback", {
  skip_without_program("socat")
  skip_if_no_processx()

})

test_that("UDP6 on loopback", {
  skip_without_program("socat")
  skip_if_no_processx()

})
