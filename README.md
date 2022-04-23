
# ps

> List, Query, Manipulate System Processes

<!-- badges: start -->
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/r-lib/ps/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/ps/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/ps)](https://cran.r-project.org/package=ps)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/ps)](https://www.r-pkg.org/pkg/ps)
[![Codecov test coverage](https://codecov.io/gh/r-lib/ps/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/ps?branch=main)
<!-- badges: end -->

ps implements an API to query and manipulate system processes. Most of its
code is based on the [psutil](https://github.com/giampaolo/psutil) Python
package.


-   [Installation](#installation)
-   [Supported platforms](#supported-platforms)
-   [Listing all processes](#listing-all-processes)
-   [Process API](#process-api)
    -   [Query functions](#query-functions)
    -   [Process manipulation](#process-manipulation)
-   [Finished and zombie processes](#finished-and-zombie-processes)
-   [Pid reuse](#pid-reuse)
-   [Recipes](#recipes)
    -   [Find process by name](#find-process-by-name)
    -   [Wait for a process to finish](#wait-for-a-process-to-finish)
    -   [Wait for several processes to
        finish](#wait-for-several-processes-to-finish)
    -   [Kill process tree](#kill-process-tree)
    -   [Terminate children](#terminate-children)
    -   [Filtering and sorting
        processes](#filtering-and-sorting-processes)
-   [Code of Conduct](#code-of-conduct)
-   [License](#license)

## Installation

You can install the released version of ps from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ps")
```

## Supported platforms

ps currently supports Windows (from Vista), macOS and Linux systems. On
unsupported platforms the package can be installed and loaded, but all
of its functions fail with an error of class `"not_implemented"`.

## Listing all processes

`ps_pids()` returns all process ids on the system. This can be useful to
iterate over all processes.

``` r
library(ps)
ps_pids()[1:20]
```

    ##  [1]   0   1 370 371 372 497 498 500 501 502 504 506 508 509 512 517 521 523 524 528

`ps()` returns a data frame (tibble if you have the tibble package
available), with data about each process. It contains a handle to each
process, in the `ps_handle` column, you can use these to perform more
queries on the processes.

``` r
ps()
```

    ## # A tibble: 523 × 11
    ##      pid  ppid name    username status    user  system    rss     vms created             ps_handle 
    ##  * <int> <int> <chr>   <chr>    <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 32549     1 MRT     gaborcs… runni… 2.16e-4 3.24e-4 5.59e6 4.18e11 2022-04-23 14:53:35 <ps_handl>
    ##  2 32537 28512 R       gaborcs… runni… 1.04e-2 1.47e-3 1.53e8 4.19e11 2022-04-23 14:53:34 <ps_handl>
    ##  3 32494     1 mdwork… gaborcs… runni… 1.13e-3 3.82e-4 2.09e7 4.18e11 2022-04-23 14:53:33 <ps_handl>
    ##  4 32487     1 mdwork… gaborcs… runni… 7.54e-4 4.63e-4 1.38e7 4.18e11 2022-04-23 14:53:33 <ps_handl>
    ##  5 32486     1 mdwork… gaborcs… runni… 1.18e-3 5.54e-4 2.09e7 4.18e11 2022-04-23 14:53:33 <ps_handl>
    ##  6 32446  3845 Google… gaborcs… runni… 1.14e-3 4.12e-4 5.87e7 4.61e11 2022-04-23 14:53:20 <ps_handl>
    ##  7 32445  3845 Google… gaborcs… runni… 4.81e-4 1.91e-4 4.05e7 4.57e11 2022-04-23 14:53:17 <ps_handl>
    ##  8 32442  3845 Google… gaborcs… runni… 6.10e-2 6.27e-3 2.38e8 4.62e11 2022-04-23 14:53:17 <ps_handl>
    ##  9 32438  3845 Google… gaborcs… runni… 1.61e-3 5.20e-4 8.13e7 4.61e11 2022-04-23 14:52:57 <ps_handl>
    ## 10 32416     1 mdwork… gaborcs… runni… 1.50e-3 6.43e-4 2.06e7 4.18e11 2022-04-23 14:52:07 <ps_handl>
    ## # … with 513 more rows

## Process API

This is a short summary of the API. Please see the documentation of the
various methods for details, in particular regarding handles to finished
processes and pid reuse. See also “Finished and zombie processes” and
“pid reuse” below.

`ps_handle(pid)` creates a process handle for the supplied process id.
If `pid` is omitted, a handle to the calling process is returned:

``` r
p <- ps_handle()
p
```

    ## <ps::ps_handle> PID=32537, NAME=R, AT=2022-04-23 14:53:34

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 32537

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2022-04-23 14:53:34 GMT"

The process id and the creation time uniquely identify a process in a
system. ps uses them to make sure that it reports information about, and
manipulates the correct process.

`ps_is_running(p)` returns whether `p` is still running. It handles pid
reuse safely.

``` r
ps_is_running(p)
```

    ## [1] TRUE

`ps_ppid(p)` returns the pid of the parent of `p`.

``` r
ps_ppid(p)
```

    ## [1] 28512

`ps_parent(p)` returns a process handle to the parent process of `p`.

``` r
ps_parent(p)
```

    ## <ps::ps_handle> PID=28512, NAME=R, AT=2022-04-23 12:33:17

`ps_name(p)` returns the name of the program `p` is running.

``` r
ps_name(p)
```

    ## [1] "R"

`ps_exe(p)` returns the full path to the executable the `p` is running.

``` r
ps_exe(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/bin/exec/R"

`ps_cmdline(p)` returns the command line (executable and arguments) of
`p`.

``` r
ps_cmdline(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/bin/exec/R"            
    ## [2] "--slave"                                                                            
    ## [3] "--no-save"                                                                          
    ## [4] "--no-restore"                                                                       
    ## [5] "-f"                                                                                 
    ## [6] "/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T//RtmpNW4j0T/callr-scr-6f607ecfcf18"

`ps_status(p)` returns the status of the process. Possible values are OS
dependent, but typically there is `"running"` and `"stopped"`.

``` r
ps_status(p)
```

    ## [1] "running"

`ps_username(p)` returns the name of the user the process belongs to.

``` r
ps_username(p)
```

    ## [1] "gaborcsardi"

`ps_uids(p)` and `ps_gids(p)` return the real, effective and saved user
ids of the process. They are only implemented on POSIX systems.

``` r
if (ps_os_type()[["POSIX"]]) ps_uids(p)
```

    ##      real effective     saved 
    ##       501       501       501

``` r
if (ps_os_type()[["POSIX"]]) ps_gids(p)
```

    ##      real effective     saved 
    ##        20        20        20

`ps_cwd(p)` returns the current working directory of the process.

``` r
ps_cwd(p)
```

    ## [1] "/Users/gaborcsardi/works/ps"

`ps_terminal(p)` returns the name of the terminal of the process, if
any. For processes without a terminal, and on Windows it returns
`NA_character_`.

``` r
ps_terminal(p)
```

    ## [1] NA

`ps_environ(p)` returns the environment variables of the process.
`ps_environ_raw(p)` does the same, in a different form. Typically they
reflect the environment variables at the start of the process.

``` r
ps_environ(p)[c("TERM", "USER", "SHELL", "R_HOME")]
```

    ## TERM                          xterm-256color
    ## USER                          gaborcsardi
    ## SHELL                         /bin/zsh
    ## R_HOME                        /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources

`ps_num_threads(p)` returns the current number of threads of the
process.

``` r
ps_num_threads(p)
```

    ## [1] 3

`ps_cpu_times(p)` returns the CPU times of the process, similarly to
`proc.time()`.

``` r
ps_cpu_times(p)
```

    ##            user          system   children_user children_system 
    ##     0.011758996     0.001636022              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##          rss          vms      pfaults      pageins 
    ##    161431552 419165847552        10659           70

`ps_children(p)` lists all child processes (potentially recursively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=32537, NAME=R, AT=2022-04-23 14:53:34

`ps_num_fds(p)` returns the number of open file descriptors (handles on
Windows):

``` r
ps_num_fds(p)
```

    ## [1] 4

``` r
f <- file(tmp <- tempfile(), "w")
ps_num_fds(p)
```

    ## [1] 5

``` r
close(f)
unlink(tmp)
```

`ps_open_files(p)` lists all open files:

``` r
ps_open_files(p)
```

    ## # A tibble: 2 × 2
    ##      fd path                                                                                      
    ##   <int> <chr>                                                                                     
    ## 1     0 /dev/null                                                                                 
    ## 2     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpNW4j0T/callr-scr-6f607ecfcf18

``` r
f <- file(tmp <- tempfile(), "w")
ps_open_files(p)
```

    ## # A tibble: 3 × 2
    ##      fd path                                                                                      
    ##   <int> <chr>                                                                                     
    ## 1     0 /dev/null                                                                                 
    ## 2     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpNW4j0T/callr-scr-6f607ecfcf18
    ## 3     4 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpEihKx2/file7f194ed30fc9

``` r
close(f)
unlink(tmp)
ps_open_files(p)
```

    ## # A tibble: 2 × 2
    ##      fd path                                                                                      
    ##   <int> <chr>                                                                                     
    ## 1     0 /dev/null                                                                                 
    ## 2     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpNW4j0T/callr-scr-6f607ecfcf18

### Process manipulation

`ps_suspend(p)` suspends (stops) the process. On POSIX it sends a
SIGSTOP signal. On Windows it stops all threads.

`ps_resume(p)` resumes the process. On POSIX it sends a SIGCONT signal.
On Windows it resumes all stopped threads.

`ps_send_signal(p)` sends a signal to the process. It is implemented on
POSIX systems only. It makes an effort to work around pid reuse.

`ps_terminate(p)` send SIGTERM to the process. On POSIX systems only.

`ps_kill(p)` terminates the process. Sends `SIGKILL` on POSIX systems,
uses `TerminateProcess()` on Windows. It make an effort to work around
pid reuse.

`ps_interrupt(p)` interrupts a process. It sends a `SIGINT` signal on
POSIX systems, and it can send a CTRL+C or a CTRL+BREAK event on
Windows.

## Finished and zombie processes

ps handles finished and Zombie processes as much as possible.

The essential `ps_pid()`, `ps_create_time()`, `ps_is_running()`
functions and the `format()` and `print()` methods work for all
processes, including finished and zombie processes. Other functions fail
with an error of class `"no_such_process"` for finished processes.

The `ps_ppid()`, `ps_parent()`, `ps_children()`, `ps_name()`,
`ps_status()`, `ps_username()`, `ps_uids()`, `ps_gids()`,
`ps_terminal()`, `ps_children()` and the signal sending functions work
properly for zombie processes. Other functions fail with
`"zombie_process"` error.

## Pid reuse

ps functions handle pid reuse as well as technically possible.

The query functions never return information about the wrong process,
even if the process has finished and its process id was re-assigned.

On Windows, the process manipulation functions never manipulate the
wrong process.

On POSIX systems, this is technically impossible, it is not possible to
send a signal to a process without creating a race condition. In ps the
time window of the race condition is very small, a few microseconds, and
the process would need to finish, *and* the OS would need to reuse its
pid within this time window to create problems. This is very unlikely to
happen.

## Recipes

In the spirit of [psutil
recipes](http://psutil.readthedocs.io/en/latest/#recipes).

### Find process by name

Using `ps()` and dplyr:

``` r
library(dplyr)
find_procs_by_name <- function(name) {
  ps() %>%
    filter(name == !!name)  %>%
    pull(ps_handle)
}

find_procs_by_name("R")
```

    ## [[1]]
    ## <ps::ps_handle> PID=32537, NAME=R, AT=2022-04-23 14:53:34
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=28512, NAME=R, AT=2022-04-23 12:33:17
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=9278, NAME=R, AT=2022-04-19 09:56:57

Without creating the full table of processes:

``` r
find_procs_by_name <- function(name) {
  procs <- lapply(ps_pids(), function(p) {
    tryCatch({
      h <- ps_handle(p)
      if (ps_name(h) == name) h else NULL },
      no_such_process = function(e) NULL,
      access_denied = function(e) NULL
    )
  })
  procs[!vapply(procs, is.null, logical(1))]
  }

find_procs_by_name("R")
```

    ## [[1]]
    ## <ps::ps_handle> PID=9278, NAME=R, AT=2022-04-19 09:56:57
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=28512, NAME=R, AT=2022-04-23 12:33:17
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=32537, NAME=R, AT=2022-04-23 14:53:34

### Wait for a process to finish

On POSIX, there is no good way to wait for non-child processes to
finish, so we need to write a sleep-wait loop to do it. (On Windows, and
BSD systems, including macOS, there are better solutions.)

``` r
as_secs <- function(x) as.numeric(x, units = "secs")

wait_for_process <- function(proc, timeout = Inf, sleep = 0.1) {
  sleep <- as_secs(sleep)
  deadline <- Sys.time() + timeout
  while (ps_is_running(proc) && (timeout == Inf || Sys.time() < deadline)) {
    to <- min(as_secs(deadline - Sys.time()), sleep)
    Sys.sleep(to)
  }
  ! ps_is_running(proc)
}

px <- processx::process$new("sleep", "2")
p <- ps_handle(px$get_pid())
wait_for_process(p, 1)
```

    ## [1] FALSE

``` r
wait_for_process(p)
```

    ## [1] TRUE

### Wait for several processes to finish

This is similar, but we need to wait on all processes in a loop.

``` r
wait_for_processes <- function(procs, timeout = Inf) {
  gone <- list()
  alive <- procs
  deadline <- Sys.time() + timeout

  check_gone <- function(proc, timeout) {
    proc_gone <- wait_for_process(proc, timeout = timeout)
    if (proc_gone) {
      gone <<- c(gone, list(proc))
      alive <<- setdiff(alive, list(proc))
    }
  }

  while (length(alive)) {
    if (timeout <= 0) break
    for (proc in alive) {
      max_timeout <- 1 / length(alive)
      if (timeout != Inf) {
        timeout <- min(as_secs(deadline - Sys.time()), max_timeout)
        if (timeout <= 0) break
        check_gone(proc, timeout)
      } else {
        check_gone(proc, max_timeout)
      }
    }
  }
  list(gone = gone, alive = alive)
}

px1 <- processx::process$new("sleep", "10")
px2 <- processx::process$new("sleep", "10")
px3 <- processx::process$new("sleep", "1")
px4 <- processx::process$new("sleep", "1")

p1 <- ps_handle(px1$get_pid())
p2 <- ps_handle(px2$get_pid())
p3 <- ps_handle(px3$get_pid())
p4 <- ps_handle(px4$get_pid())

wait_for_processes(list(p1, p2, p3, p4), timeout = 2)
```

    ## $gone
    ## $gone[[1]]
    ## <ps::ps_handle> PID=32556, NAME=???, AT=2022-04-23 14:53:37
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=32555, NAME=???, AT=2022-04-23 14:53:37
    ## 
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=32553, NAME=sleep, AT=2022-04-23 14:53:37
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=32554, NAME=sleep, AT=2022-04-23 14:53:37

### Kill process tree

This sends a signal, so it’ll only work on Unix. Use `ps_kill()` instead
of `ps_send_signal()` on Windows.

``` r
kill_proc_tree <- function(pid, sig = signals()$SIGTERM,
                           include_parent = TRUE) {
  if (pid == Sys.getpid() && include_parent) stop("I refuse to kill myself")
  parent <- ps_handle(pid)
  children <- ps_children(parent, recursive = TRUE)
  if (include_parent) children <- c(children, parent)
  for (p in children) ps_send_signal(p, sig)
  wait_for_processes(children, timeout = 0.1)
}

p1 <- processx::process$new("sleep", "10")
p2 <- processx::process$new("sleep", "10")
p3 <- processx::process$new("sleep", "10")
kill_proc_tree(Sys.getpid(), include_parent = FALSE)
```

    ## $gone
    ## $gone[[1]]
    ## <ps::ps_handle> PID=32553, NAME=???, AT=2022-04-23 14:53:37
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=32554, NAME=???, AT=2022-04-23 14:53:37
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=32561, NAME=???, AT=2022-04-23 14:53:39
    ## 
    ## $gone[[4]]
    ## <ps::ps_handle> PID=32562, NAME=???, AT=2022-04-23 14:53:39
    ## 
    ## $gone[[5]]
    ## <ps::ps_handle> PID=32563, NAME=???, AT=2022-04-23 14:53:39
    ## 
    ## 
    ## $alive
    ## list()

### Terminate children

Note, that some R IDEs, including RStudio, run a multithreaded R
process, and other threads may start processes as well.
`reap_children()` will clean up all these as well, potentially causing
the IDE to misbehave or crash.

``` r
reap_children <- function(timeout = 3) {
  procs <- ps_children(ps_handle())

  ## SIGTERM
  lapply(procs, ps_terminate)

  ga <- wait_for_processes(procs, timeout = timeout)

  ## SIGKILL to the survivers
  if (length(ga$alive)) lapply(ga$alive, ps_kill)

  ga2 <- wait_for_processes(ga$alive, timeout = timeout)

  ## Some might still survive
  list(gone = c(ga$gone, ga2$gone), alive = ga2$alive)
}

pxs <- replicate(3, processx::process$new("sleep", "3"))
reap_children()
```

    ## $gone
    ## $gone[[1]]
    ## <ps::ps_handle> PID=32564, NAME=???, AT=2022-04-23 14:53:39
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=32565, NAME=???, AT=2022-04-23 14:53:39
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=32566, NAME=???, AT=2022-04-23 14:53:39
    ## 
    ## 
    ## $alive
    ## list()

### Filtering and sorting processes

Process name ending with “sh”:

``` r
ps() %>%
  filter(grepl("sh$", name))
```

    ## # A tibble: 21 × 11
    ##      pid  ppid name  username   status    user  system    rss     vms created             ps_handle 
    ##    <int> <int> <chr> <chr>      <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 31628 31296 zsh   gaborcsar… runni… 3.11e-5 1.59e-4 3.34e6 4.19e11 2022-04-23 14:46:16 <ps_handl>
    ##  2 31296 31295 zsh   gaborcsar… runni… 2.21e-3 1.03e-3 1.17e7 4.19e11 2022-04-23 14:46:15 <ps_handl>
    ##  3 31176 30844 zsh   gaborcsar… runni… 1.87e-4 8.07e-4 3.46e6 4.19e11 2022-04-23 14:45:56 <ps_handl>
    ##  4 30844 30843 zsh   gaborcsar… runni… 3.14e-3 2.03e-3 1.26e7 4.19e11 2022-04-23 14:45:55 <ps_handl>
    ##  5 28331 27999 zsh   gaborcsar… runni… 9.40e-5 4.45e-4 1.79e6 4.19e11 2022-04-23 12:32:09 <ps_handl>
    ##  6 27999 27998 zsh   gaborcsar… runni… 2.91e-3 1.83e-3 8.00e6 4.19e11 2022-04-23 12:32:09 <ps_handl>
    ##  7 27624 27292 zsh   gaborcsar… runni… 2.76e-5 1.55e-4 1.03e6 4.19e11 2022-04-23 12:09:06 <ps_handl>
    ##  8 27292 27291 zsh   gaborcsar… runni… 2.38e-3 1.38e-3 1.87e6 4.19e11 2022-04-23 12:09:05 <ps_handl>
    ##  9 20941 20609 zsh   gaborcsar… runni… 1.82e-5 1.03e-4 3.44e5 4.19e11 2022-04-22 14:30:13 <ps_handl>
    ## 10 20609 20608 zsh   gaborcsar… runni… 2.26e-3 1.37e-3 7.54e5 4.19e11 2022-04-22 14:30:12 <ps_handl>
    ## # … with 11 more rows

Processes owned by user:

``` r
ps() %>%
  filter(username == Sys.info()[["user"]]) %>%
  select(pid, name)
```

    ## # A tibble: 309 × 2
    ##      pid name                           
    ##    <int> <chr>                          
    ##  1 32567 Google Chrome Helper (Renderer)
    ##  2 32558 Google Chrome Helper (Renderer)
    ##  3 32537 R                              
    ##  4 32494 mdworker_shared                
    ##  5 32487 mdworker_shared                
    ##  6 32486 mdworker_shared                
    ##  7 32445 Google Chrome Helper           
    ##  8 32442 Google Chrome Helper (Renderer)
    ##  9 32438 Google Chrome Helper (Renderer)
    ## 10 32416 mdworker_shared                
    ## # … with 299 more rows

Processes consuming more than 100MB of memory:

``` r
ps() %>%
  filter(rss > 100 * 1024 * 1024)
```

    ## # A tibble: 17 × 11
    ##      pid  ppid name    username status    user  system    rss     vms created             ps_handle 
    ##    <int> <int> <chr>   <chr>    <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 32558  3845 Google… gaborcs… runni… 8.41e-3 1.35e-3 1.91e8 4.66e11 2022-04-23 14:53:39 <ps_handl>
    ##  2 32537 28512 R       gaborcs… runni… 1.95e-2 6.44e-3 1.81e8 4.20e11 2022-04-23 14:53:34 <ps_handl>
    ##  3 32442  3845 Google… gaborcs… runni… 6.14e-2 6.36e-3 2.38e8 4.62e11 2022-04-23 14:53:17 <ps_handl>
    ##  4 29432     1 Amazon… gaborcs… runni… 2.87e-1 5.92e-2 2.31e8 3.67e10 2022-04-23 14:38:33 <ps_handl>
    ##  5 28512 27999 R       gaborcs… runni… 4.73e-2 8.86e-3 2.78e8 4.19e11 2022-04-23 12:33:17 <ps_handl>
    ##  6 28456  3845 Google… gaborcs… runni… 6.22e-2 1.02e-2 1.69e8 4.62e11 2022-04-23 12:32:21 <ps_handl>
    ##  7 25580  3845 Google… gaborcs… runni… 1.47e-1 2.85e-2 1.12e8 4.66e11 2022-04-23 11:19:18 <ps_handl>
    ##  8 25386  3845 Google… gaborcs… runni… 7.38e-1 1.00e-1 2.75e8 4.66e11 2022-04-23 11:05:43 <ps_handl>
    ##  9  8796  3845 Google… gaborcs… runni… 2.87e+0 6.11e-1 2.26e8 4.66e11 2022-04-22 10:56:48 <ps_handl>
    ## 10  3915  3845 Google… gaborcs… runni… 1.18e+1 1.38e+0 5.21e8 4.66e11 2022-04-22 07:40:30 <ps_handl>
    ## 11  3904  3845 Google… gaborcs… runni… 1.49e+0 4.16e-1 2.08e8 4.61e11 2022-04-22 07:40:27 <ps_handl>
    ## 12  3876  3845 Google… gaborcs… runni… 1.75e+0 4.14e-1 1.15e8 4.72e11 2022-04-22 07:40:24 <ps_handl>
    ## 13  3865  3845 Google… gaborcs… runni… 5.77e-1 1.49e-1 1.77e8 4.66e11 2022-04-22 07:40:21 <ps_handl>
    ## 14  3859  3845 Google… gaborcs… runni… 3.32e+1 1.94e+1 2.35e8 4.54e11 2022-04-22 07:40:21 <ps_handl>
    ## 15  3845     1 Google… gaborcs… runni… 2.71e+1 9.36e+0 5.18e8 4.53e11 2022-04-22 07:40:20 <ps_handl>
    ## 16 97901 94806 Emacs-… gaborcs… runni… 7.64e+0 1.12e+0 1.16e8 4.20e11 2022-04-19 08:35:44 <ps_handl>
    ## 17 14121     1 iTerm2  gaborcs… runni… 2.04e+2 3.82e+1 5.47e8 4.21e11 2022-04-03 23:28:13 <ps_handl>

Top 3 memory consuming processes:

``` r
ps() %>%
  top_n(3, rss) %>%
  arrange(desc(rss))
```

    ## # A tibble: 3 × 11
    ##     pid  ppid name        username status  user system    rss     vms created             ps_handle 
    ##   <int> <int> <chr>       <chr>    <chr>  <dbl>  <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ## 1 14121     1 iTerm2      gaborcs… runni… 204.   38.2  5.47e8 4.21e11 2022-04-03 23:28:13 <ps_handl>
    ## 2  3915  3845 Google Chr… gaborcs… runni…  11.8   1.38 5.21e8 4.66e11 2022-04-22 07:40:30 <ps_handl>
    ## 3  3845     1 Google Chr… gaborcs… runni…  27.1   9.36 4.85e8 4.53e11 2022-04-22 07:40:20 <ps_handl>

Top 3 processes which consumed the most CPU time:

``` r
ps() %>%
  mutate(cpu_time = user + system) %>%
  top_n(3, cpu_time) %>%
  arrange(desc(cpu_time)) %>%
  select(pid, name, cpu_time)
```

    ## # A tibble: 3 × 3
    ##     pid name                 cpu_time
    ##   <int> <chr>                   <dbl>
    ## 1 14121 iTerm2                  242. 
    ## 2 10419 keybase                 102. 
    ## 3 10367 Keybase Helper (GPU)     62.3

## Code of Conduct

Please note that the ps project is released with a [Contributor Code of
Conduct](https://ps.r-lib.org/CODE_OF_CONDUCT.html). By contributing to
this project, you agree to abide by its terms.

## License

MIT © RStudio
