
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

    ##  [1]   0   1 497 498 500 501 502 505 506 508 509 512 514 515 516 518 522 523 524 525

`ps()` returns a data frame (tibble if you have the tibble package
available), with data about each process. It contains a handle to each
process, in the `ps_handle` column, you can use these to perform more
queries on the processes.

``` r
ps()
```

    ## # A tibble: 520 × 11
    ##      pid  ppid name    username status    user  system    rss     vms created             ps_handle 
    ##  * <int> <int> <chr>   <chr>    <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 56528 68177 R       gaborcs… runni… 0.00993 1.54e-3 1.42e8 4.19e11 2022-03-02 13:30:10 <ps_handl>
    ##  2 56305 68005 R       gaborcs… runni… 0.0153  1.90e-3 1.39e8 4.19e11 2022-03-02 13:28:54 <ps_handl>
    ##  3 54044  9722 Google… gaborcs… runni… 0.00121 5.00e-4 4.15e7 4.61e11 2022-03-02 13:16:37 <ps_handl>
    ##  4 54028  9722 Google… gaborcs… runni… 0.00557 1.30e-3 8.59e7 4.61e11 2022-03-02 13:15:06 <ps_handl>
    ##  5 54027  9722 Google… gaborcs… runni… 0.00585 1.37e-3 8.35e7 4.61e11 2022-03-02 13:15:05 <ps_handl>
    ##  6 54026  9722 Google… gaborcs… runni… 0.0294  4.42e-3 1.27e8 4.61e11 2022-03-02 13:15:04 <ps_handl>
    ##  7 54025  9722 Google… gaborcs… runni… 0.00587 1.82e-3 7.81e7 4.61e11 2022-03-02 13:15:04 <ps_handl>
    ##  8 54024  9722 Google… gaborcs… runni… 0.0271  1.18e-2 8.72e7 4.61e11 2022-03-02 13:15:04 <ps_handl>
    ##  9 54023  9722 Google… gaborcs… runni… 0.00818 1.76e-3 9.21e7 4.61e11 2022-03-02 13:15:04 <ps_handl>
    ## 10 54022  9722 Google… gaborcs… runni… 0.00794 1.84e-3 9.35e7 4.61e11 2022-03-02 13:15:04 <ps_handl>
    ## # … with 510 more rows

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

    ## <ps::ps_handle> PID=56528, NAME=R, AT=2022-03-02 13:30:10

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 56528

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2022-03-02 13:30:10 GMT"

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

    ## [1] 68177

`ps_parent(p)` returns a process handle to the parent process of `p`.

``` r
ps_parent(p)
```

    ## <ps::ps_handle> PID=68177, NAME=zsh, AT=2022-03-02 10:44:55

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
    ## [2] "--no-echo"                                                              
    ## [3] "--no-restore"                                                           
    ## [4] "-e"                                                                     
    ## [5] "rmarkdown::render(\"README.Rmd\")"

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

    ## [1] "/dev/ttys012"

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

    ## [1] 4

`ps_cpu_times(p)` returns the CPU times of the process, similarly to
`proc.time()`.

``` r
ps_cpu_times(p)
```

    ##            user          system   children_user children_system 
    ##     0.011270084     0.001720491              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##          rss          vms      pfaults      pageins 
    ##    148455424 418888728576         9869           70

`ps_children(p)` lists all child processes (potentially recursively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=56528, NAME=R, AT=2022-03-02 13:30:10
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=68198, NAME=zsh, AT=2022-03-02 10:44:55
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=68260, NAME=Emacs-arm64-11_2, AT=2022-03-02 10:44:59

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

    ## # A tibble: 4 × 2
    ##      fd path                                                                       
    ##   <int> <chr>                                                                      
    ## 1     0 /dev/ttys012                                                               
    ## 2     1 /dev/ttys012                                                               
    ## 3     2 /dev/ttys012                                                               
    ## 4     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/Rscriptdcd0.BKgQZ2

``` r
f <- file(tmp <- tempfile(), "w")
ps_open_files(p)
```

    ## # A tibble: 5 × 2
    ##      fd path                                                                                
    ##   <int> <chr>                                                                               
    ## 1     0 /dev/ttys012                                                                        
    ## 2     1 /dev/ttys012                                                                        
    ## 3     2 /dev/ttys012                                                                        
    ## 4     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/Rscriptdcd0.BKgQZ2         
    ## 5     4 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpsC7fip/filedcd0299de121

``` r
close(f)
unlink(tmp)
ps_open_files(p)
```

    ## # A tibble: 4 × 2
    ##      fd path                                                                       
    ##   <int> <chr>                                                                      
    ## 1     0 /dev/ttys012                                                               
    ## 2     1 /dev/ttys012                                                               
    ## 3     2 /dev/ttys012                                                               
    ## 4     3 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/Rscriptdcd0.BKgQZ2

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
    ## <ps::ps_handle> PID=56528, NAME=R, AT=2022-03-02 13:30:10
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=56305, NAME=R, AT=2022-03-02 13:28:54
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=68671, NAME=R, AT=2022-03-02 10:47:28
    ## 
    ## [[4]]
    ## <ps::ps_handle> PID=63850, NAME=R, AT=2022-03-02 10:00:27

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
    ## <ps::ps_handle> PID=56305, NAME=R, AT=2022-03-02 13:28:54
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=56528, NAME=R, AT=2022-03-02 13:30:10
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=63850, NAME=R, AT=2022-03-02 10:00:27
    ## 
    ## [[4]]
    ## <ps::ps_handle> PID=68671, NAME=R, AT=2022-03-02 10:47:28

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
    ## <ps::ps_handle> PID=56547, NAME=???, AT=2022-03-02 13:30:13
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=56546, NAME=???, AT=2022-03-02 13:30:13
    ## 
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=56544, NAME=sleep, AT=2022-03-02 13:30:13
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=56545, NAME=sleep, AT=2022-03-02 13:30:13

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
    ## <ps::ps_handle> PID=56544, NAME=???, AT=2022-03-02 13:30:13
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=56545, NAME=???, AT=2022-03-02 13:30:13
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=56548, NAME=???, AT=2022-03-02 13:30:15
    ## 
    ## $gone[[4]]
    ## <ps::ps_handle> PID=56549, NAME=???, AT=2022-03-02 13:30:15
    ## 
    ## $gone[[5]]
    ## <ps::ps_handle> PID=56550, NAME=???, AT=2022-03-02 13:30:15
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
    ## <ps::ps_handle> PID=56551, NAME=???, AT=2022-03-02 13:30:15
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=56552, NAME=???, AT=2022-03-02 13:30:15
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=56553, NAME=???, AT=2022-03-02 13:30:15
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

    ## # A tibble: 20 × 11
    ##      pid  ppid name        username    status     user   system     rss      vms created            
    ##    <int> <int> <chr>       <chr>       <chr>     <dbl>    <dbl>   <dbl>    <dbl> <dttm>             
    ##  1 68613 68592 zsh         gaborcsardi runni…  3.00e-5  1.43e-4  1.02e6  4.19e11 2022-03-02 10:47:28
    ##  2 68592 68591 zsh         gaborcsardi runni…  9.50e-4  7.54e-4  1.87e6  4.19e11 2022-03-02 10:47:28
    ##  3 68198 68177 zsh         gaborcsardi runni…  1.18e-3  4.90e-3  2.82e6  4.19e11 2022-03-02 10:44:55
    ##  4 68177 68176 zsh         gaborcsardi runni…  1.34e-2  1.03e-2  9.83e6  4.19e11 2022-03-02 10:44:55
    ##  5 68026 68005 zsh         gaborcsardi runni…  9.37e-4  3.98e-3  2.79e6  4.19e11 2022-03-02 10:44:20
    ##  6 68005 68004 zsh         gaborcsardi runni…  9.82e-3  7.35e-3  9.99e6  4.19e11 2022-03-02 10:44:20
    ##  7 63792 63771 zsh         gaborcsardi runni…  3.07e-5  1.48e-4  1.02e6  4.19e11 2022-03-02 10:00:27
    ##  8 63771 63770 zsh         gaborcsardi runni…  9.31e-4  9.37e-4  1.87e6  4.19e11 2022-03-02 10:00:26
    ##  9 54905 54884 zsh         gaborcsardi runni…  1.44e-3  6.15e-3  1.02e6  4.19e11 2022-03-02 09:27:48
    ## 10 54884 54883 zsh         gaborcsardi runni…  1.37e-2  1.23e-2  1.98e6  4.19e11 2022-03-02 09:27:48
    ## 11 54148 54127 zsh         gaborcsardi runni…  2.17e-3  9.13e-3  1.02e6  4.19e11 2022-03-02 09:24:05
    ## 12 54127 54126 zsh         gaborcsardi runni…  2.09e-2  1.93e-2  1.98e6  4.19e11 2022-03-02 09:24:05
    ## 13 52553 51524 ssh         gaborcsardi runni…  1.19e-3  1.42e-3  1.41e6  4.18e11 2022-03-02 07:55:53
    ## 14 51545 51524 zsh         gaborcsardi runni…  3.20e-5  1.74e-4  1.02e6  4.19e11 2022-03-01 22:43:44
    ## 15 51524 51523 zsh         gaborcsardi runni…  1.41e-3  1.43e-3  1.88e6  4.19e11 2022-03-01 22:43:44
    ## 16 49995     1 ReportCrash root        runni… NA       NA       NA      NA       2022-03-01 20:15:37
    ## 17 11004 10988 zsh         gaborcsardi runni…  4.50e-5  2.61e-4  9.99e5  4.19e11 2022-03-01 09:09:01
    ## 18 10988 10987 zsh         gaborcsardi runni…  1.06e-3  9.49e-4  1.75e6  4.19e11 2022-03-01 09:09:01
    ## 19  9643     1 Plash       gaborcsardi runni…  7.92e-2  7.12e-2  4.10e7  4.20e11 2022-03-01 08:20:56
    ## 20  2026     1 ReportCrash gaborcsardi runni…  2.89e-2  2.61e-2  1.18e7  4.18e11 2022-03-01 07:36:57
    ## # … with 1 more variable: ps_handle <I<list>>

Processes owned by user:

``` r
ps() %>%
  filter(username == Sys.info()[["user"]]) %>%
  select(pid, name)
```

    ## # A tibble: 334 × 2
    ##      pid name                           
    ##    <int> <chr>                          
    ##  1 56528 R                              
    ##  2 56305 R                              
    ##  3 54044 Google Chrome Helper (Renderer)
    ##  4 54028 Google Chrome Helper (Renderer)
    ##  5 54027 Google Chrome Helper (Renderer)
    ##  6 54026 Google Chrome Helper (Renderer)
    ##  7 54025 Google Chrome Helper (Renderer)
    ##  8 54024 Google Chrome Helper (Renderer)
    ##  9 54023 Google Chrome Helper (Renderer)
    ## 10 54022 Google Chrome Helper (Renderer)
    ## # … with 324 more rows

Processes consuming more than 100MB of memory:

``` r
ps() %>%
  filter(rss > 100 * 1024 * 1024)
```

    ## # A tibble: 24 × 11
    ##      pid  ppid name     username status   user  system    rss     vms created             ps_handle 
    ##    <int> <int> <chr>    <chr>    <chr>   <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 56528 68177 R        gaborcs… runni… 0.0194 0.00651 1.69e8 4.19e11 2022-03-02 13:30:10 <ps_handl>
    ##  2 56305 68005 R        gaborcs… runni… 0.0153 0.00191 1.39e8 4.19e11 2022-03-02 13:28:54 <ps_handl>
    ##  3 54026  9722 Google … gaborcs… runni… 0.0294 0.00442 1.27e8 4.61e11 2022-03-02 13:15:04 <ps_handl>
    ##  4 54021  9722 Google … gaborcs… runni… 0.0788 0.0114  2.24e8 4.62e11 2022-03-02 13:15:03 <ps_handl>
    ##  5 54018  9722 Google … gaborcs… runni… 0.120  0.0192  1.46e8 4.61e11 2022-03-02 13:14:53 <ps_handl>
    ##  6 54017  9722 Google … gaborcs… runni… 0.0496 0.0133  1.34e8 4.61e11 2022-03-02 13:14:53 <ps_handl>
    ##  7 54014  9722 Google … gaborcs… runni… 0.0147 0.00493 1.13e8 4.61e11 2022-03-02 13:14:52 <ps_handl>
    ##  8 50899     1 Amazon … gaborcs… runni… 0.404  0.0849  2.41e8 3.67e10 2022-03-02 13:04:19 <ps_handl>
    ##  9 18649  9722 Google … gaborcs… runni… 0.0719 0.00914 1.57e8 4.61e11 2022-03-02 11:56:06 <ps_handl>
    ## 10 68671 68592 R        gaborcs… runni… 0.755  0.0170  2.15e8 4.19e11 2022-03-02 10:47:28 <ps_handl>
    ## # … with 14 more rows

Top 3 memory consuming processes:

``` r
ps() %>%
  top_n(3, rss) %>%
  arrange(desc(rss))
```

    ## # A tibble: 3 × 11
    ##     pid  ppid name        username status  user system    rss     vms created             ps_handle 
    ##   <int> <int> <chr>       <chr>    <chr>  <dbl>  <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ## 1 10052     1 iTerm2      gaborcs… runni…  27.2   5.80 7.11e8 4.21e11 2022-03-01 08:39:42 <ps_handl>
    ## 2  9774  9722 Google Chr… gaborcs… runni…  20.6   2.10 6.54e8 4.70e11 2022-03-01 08:21:36 <ps_handl>
    ## 3 12106 12105 QEMULaunch… gaborcs… runni…  39.6 102.   4.17e8 4.28e11 2022-03-01 10:01:40 <ps_handl>

Top 3 processes which consumed the most CPU time:

``` r
ps() %>%
  mutate(cpu_time = user + system) %>%
  top_n(3, cpu_time) %>%
  arrange(desc(cpu_time)) %>%
  select(pid, name, cpu_time)
```

    ## # A tibble: 3 × 3
    ##     pid name                       cpu_time
    ##   <int> <chr>                         <dbl>
    ## 1 12106 QEMULauncher                  141. 
    ## 2  9732 Google Chrome Helper (GPU)    114. 
    ## 3 12103 UTM                            85.5

## Code of Conduct

Please note that the ps project is released with a [Contributor Code of
Conduct](http://ps.r-lib.org/CODE_OF_CONDUCT.html). By contributing to
this project, you agree to abide by its terms.

## License

MIT © RStudio
