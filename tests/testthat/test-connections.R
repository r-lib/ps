
test_that("empty set", {
  px <- processx::process$new(
    px(), c("sleep", "5"),
    poll_connection = FALSE)
  on.exit(cleanup_process(px), add = TRUE)
  pid <- px$get_pid()
  p <- ps_handle(pid)

  cl <- ps_connections(p)
  expect_equal(nrow(cl), 0)
  expect_s3_class(cl, "data.frame")
  expect_s3_class(cl, "tbl")
  expect_equal(
    names(cl),
    c("fd", "family", "type", "laddr", "lport", "raddr", "rport", "state"))
})

test_that("UNIX sockets", {
  if (!ps_os_type()[["POSIX"]]) skip("No UNIX sockets")

  px <- processx::process$new(px(), c("sleep", "5"), stdout = "|")
  on.exit(cleanup_process(px), add = TRUE)
  pid <- px$get_pid()
  p <- ps_handle(pid)

  cl <- ps_connections(p)
  expect_equal(nrow(cl), 1)
  expect_s3_class(cl, "data.frame")
  expect_s3_class(cl, "tbl")
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
    "socat", c("-", paste0("UNIX-LISTEN:", sfile)), stdin = "|")
  on.exit(cleanup_process(nc), add = TRUE)
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
  cx <- curl::curl(httpbin_url(), open = "r")
  on.exit({ close(cx); rm(cx); gc() }, add = TRUE)
  after <- ps_connections(ps_handle())
  new <- after[! after$lport %in% before$lport, ]
  expect_equal(new$family, "AF_INET")
  expect_equal(new$type, "SOCK_STREAM")
  expect_true(is_ipv4_address(new$laddr))
  expect_true(is.integer(new$lport))
  expect_equal(new$rport, 80L)
  expect_equal(new$state, "CONN_ESTABLISHED")
})

test_that("TCP on loopback", {
  skip_without_program("socat")
  skip_if_no_processx()

  nc <- processx::process$new(
    "socat", c("-d", "-d", "-ls", "-", "TCP4-LISTEN:0"),
    stdin = "|", stderr = "|")
  on.exit(cleanup_process(nc), add = TRUE)
  p <- nc$as_ps_handle()

  wait_for_string(nc, "listening on", timeout = 2000)

  cl <- ps_connections(p)
  cl <- cl[!is.na(cl$state) & cl$state == "CONN_LISTEN", ]
  expect_equal(nrow(cl), 1)
  expect_true(cl$state == "CONN_LISTEN")
  port <- cl$lport

  nc2 <- processx::process$new(
    "socat", c("-", paste0("TCP4-CONNECT:127.0.0.1:", port)), stdin = "|")
  on.exit(cleanup_process(nc2), add = TRUE)
  p2 <- nc2$as_ps_handle()

  deadline <- Sys.time() + as.difftime(5, units = "secs")
  while (Sys.time() < deadline &&
         ! port %in% (cl2 <- ps_connections(p2))$rport) Sys.sleep(0.1)

  cl2 <- cl2[!is.na(cl2$rport & cl2$rport == port), ]
  expect_equal(cl2$family, "AF_INET")
  expect_equal(cl2$type, "SOCK_STREAM")
  expect_equal(cl2$state, "CONN_ESTABLISHED")
})

test_that("UDP", {
  skip_if_offline()
  skip_without_program("socat")
  skip_if_no_processx()

  nc <- processx::process$new(
    "socat", c("-", "UDP4-CONNECT:8.8.8.8:53,pf=ip4"), stdin = "|")
  on.exit(cleanup_process(nc), add = TRUE)
  p <- nc$as_ps_handle()

  deadline <- Sys.time() + as.difftime(5, units = "secs")
  while (Sys.time() < deadline &&
         ! 53 %in% (cl <- ps_connections(p))$rport) {
    Sys.sleep(.1)
  }

  expect_true(deadline > Sys.time())

  cl <- cl[!is.na(cl$rport) & cl$rport == 53, ]
  expect_equal(nrow(cl), 1)
  expect_equal(cl$family, "AF_INET")
  expect_equal(cl$type, "SOCK_DGRAM")
  expect_equal(cl$raddr, "8.8.8.8")
})

test_that("UDP on loopback", {
  skip_without_program("socat")
  skip_if_no_processx()

  nc <- processx::process$new(
    "socat", c("-d", "-d", "-ls", "-", "UDP4-LISTEN:0"),
    stdin = "|", stderr = "|")
  on.exit(cleanup_process(nc), add = TRUE)
  p <- nc$as_ps_handle()

  wait_for_string(nc, "listening on", timeout = 2000)

  cl <- ps_connections(p)
  cl  <-  cl[!is.na(cl$lport) & cl$type == "SOCK_DGRAM", ]
  port <- cl$lport
  expect_equal(cl$family, "AF_INET")
  expect_equal(cl$type, "SOCK_DGRAM")

  nc2 <- processx::process$new(
    "socat", c("-", paste0("UDP4-CONNECT:127.0.0.1:", port)), stdin = "|")
  on.exit(cleanup_process(nc2), add = TRUE)
  p2 <- nc2$as_ps_handle()

  deadline <- Sys.time() + as.difftime(5, units = "secs")
  while (Sys.time() < deadline &&
         ! port %in% (cl2 <- ps_connections(p2))$rport) Sys.sleep(0.1)

  cl2 <- cl2[!is.na(cl2$rport & cl2$rport == port), ]
  expect_equal(cl2$family, "AF_INET")
  expect_equal(cl2$type, "SOCK_DGRAM")
})

test_that("TCP6", {
  skip_if_offline()
  skip_without_program("socat")
  skip_if_no_processx()
  skip_without_ipv6()
  skip_without_ipv6_connection()

  nc <- processx::process$new(
    "socat", c("-d", "-d", "-", paste0("TCP6:", ipv6_host(), ":443")),
     stdin = "|",  stderr = "|")
  on.exit(cleanup_process(nc), add = TRUE)
  p <- nc$as_ps_handle()

  wait_for_string(nc, "starting data transfer", timeout = 3000)

  cl <- ps_connections(p)
  cl <- cl[!is.na(cl$rport) & cl$rport == 443, ]
  expect_equal(nrow(cl), 1)
  expect_equal(cl$family, "AF_INET6")
  expect_equal(cl$type, "SOCK_STREAM")
})

test_that("TCP6 on loopback", {
  skip_without_program("socat")
  skip_if_no_processx()
  skip_without_ipv6()

  nc <- processx::process$new(
    "socat", c("-d", "-d", "-", "TCP6-LISTEN:0"),
    stdin = "|", stderr = "|")
  on.exit(cleanup_process(nc), add = TRUE)
  p <- nc$as_ps_handle()

  wait_for_string(nc, "listening on", timeout = 2000)

  cl <- ps_connections(p)
  cl <- cl[!is.na(cl$state) & cl$state == "CONN_LISTEN", ]
  expect_equal(nrow(cl), 1)
  expect_true(cl$state == "CONN_LISTEN")
  port <- cl$lport

  nc2 <- processx::process$new(
    "socat", c("-d", "-d", "-", paste0("TCP6-CONNECT:\\:\\:1:", port)),
    stdin = "|", stderr = "|")
  on.exit(cleanup_process(nc2), add = TRUE)
  p2 <- nc2$as_ps_handle()

  err <- FALSE
  tryCatch(
    wait_for_string(nc2, "starting data transfer", timeout = 2000),
    error = function(e) err <<- TRUE)
  if (err) skip("Could not bind to IPv6 address")

  cl2 <- ps_connections(p2)
  cl2 <- cl2[!is.na(cl2$rport & cl2$rport == port), ]
  expect_equal(cl2$family, "AF_INET6")
  expect_equal(cl2$type, "SOCK_STREAM")
})

test_that("UDP6", {
  skip_if_offline()
  skip_without_ipv6()
  skip_without_ipv6_connection()
  skip_without_program("socat")
  skip_if_no_processx()

  nc <- processx::process$new(
    "socat", c("-", "UDP6:2001\\:4860\\:4860\\:8888:53"), stdin = "|")
  on.exit(cleanup_process(nc), add = TRUE)
  p <- nc$as_ps_handle()

  deadline <- Sys.time() + as.difftime(5, units = "secs")
  while (Sys.time() < deadline &&
         ! 53 %in% (cl <- ps_connections(p))$rport) {
    Sys.sleep(.1)
  }

  expect_true(deadline > Sys.time())

  cl <- cl[!is.na(cl$rport) & cl$rport == 53, ]
  expect_equal(nrow(cl), 1)
  expect_equal(cl$family, "AF_INET6")
  expect_equal(cl$type, "SOCK_DGRAM")
  expect_match(cl$raddr, "2001:4860:4860:8888", fixed = TRUE)
})

test_that("UDP6 on loopback", {
  skip_without_program("socat")
  skip_if_no_processx()
  skip_without_ipv6()

  nc <- processx::process$new(
    "socat", c("-d", "-d", "-ls", "-", "UDP6-LISTEN:0"),
    stdin = "|", stderr = "|")
  on.exit(cleanup_process(nc), add = TRUE)
  p <- nc$as_ps_handle()

  wait_for_string(nc, "listening on", timeout = 2000)

  cl <- ps_connections(p)
  cl  <-  cl[!is.na(cl$lport) & cl$type == "SOCK_DGRAM", ]
  port <- cl$lport
  expect_equal(cl$family, "AF_INET6")
  expect_equal(cl$type, "SOCK_DGRAM")

  nc2 <- processx::process$new(
    "socat", c("-d", "-d", "-", paste0("UDP6-CONNECT:\\:\\:1:", port)),
     stdin = "|", stderr = "|")
  on.exit(cleanup_process(nc2), add = TRUE)
  p2 <- nc2$as_ps_handle()

  err <- FALSE
  tryCatch(
    wait_for_string(nc2, "starting data transfer", timeout = 2000),
    error = function(e) err <<- TRUE)
  if (err) skip("Could not bind to IPv6 address")

  cl2 <- ps_connections(p2)
  cl2 <- cl2[!is.na(cl2$rport & cl2$rport == port), ]
  expect_equal(cl2$family, "AF_INET6")
  expect_equal(cl2$type, "SOCK_DGRAM")
})
