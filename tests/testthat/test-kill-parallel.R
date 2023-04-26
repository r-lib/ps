test_that("kill_parallel() doesn't kill sequentially", {
  withr::local_envvar(c("PX_SIGTERM_SLEEP" = "200"))

  pxs <- lapply(1:20, function(...) processx::process$new(px(), c("sleep", "60")))
  ps <- lapply(pxs, function(p) ps_handle(p$get_pid()))

  ps_kill_parallel(ps, grace = 10)

  # Succeeds after 0.2 seconds rather than 4
  poll_until(
    function() {
      !any(vlapply(pxs, function(p) p$is_alive()))
    },
    timeout = 2
  )
})

# Subprocesses are timeout-polled because we don't get SIGCHLD events
# for these
test_that("kill_parallel() works with subsubprocesses", {
  N <- 5
  fn <- function(px, args, recurse, sleep) {
    if (recurse) {
      p <- callr::r_session$new()
      p$call(
        sys.function(),
        list(px, args, recurse - 1, sleep = TRUE)
      )
      if (sleep) {
        Sys.sleep(60)
      }
      p
    } else {
      callr::process$new(px, args)
    }
  }
  p <- fn(px(), c("sleep", "60"), N, FALSE)

  pid <- p$get_pid()
  id <- p$.__enclos_env__$private$tree_id
  ps <- ps::ps_find_tree(id)

  ps_kill_tree(id)
  poll_until(
    function() {
      !any(vlapply(ps, function(p) ps_is_running(p)))
    },
    timeout = 2
  )
})

test_that("kill_parallel() kills with grace", {
  skip_on_os("windows")

  withr::local_envvar(c("PX_SIGTERM_IGNORE" = "true"))

  pxs <- lapply(1:2, function(...) processx::process$new(px(), c("sleep", "60")))
  ps <- lapply(pxs, function(p) ps_handle(p$get_pid()))

  ps_kill_parallel(ps)

  # Succeeds after 0.2 seconds of grace
  poll_until(
    function() {
      !any(vlapply(pxs, function(p) p$is_alive()))
    },
    timeout = 2
  )
})

test_that("ps_parallel_kill() checks create time", {
  p <- processx::process$new(px(), c("sleep", "60"))
  real_ps <- ps::ps_handle(p$get_pid())
  ps <- ps::ps_handle(p$get_pid(), Sys.time() + 1)

  ps_kill_parallel(list(ps))
  Sys.sleep(0.1)
  expect_true(ps_is_running(real_ps))

  ps_kill_parallel(list(real_ps))
  poll_until(function() !ps_is_running(ps))
})
