
# ps

> List, Query, Manipulate System Processes

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build status](https://travis-ci.org/r-lib/ps.svg?branch=master)](https://travis-ci.org/r-lib/ps)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/r-lib/ps?branch=master&svg=true)](https://ci.appveyor.com/project/gaborcsardi/ps)
[![CRAN status](https://www.r-pkg.org/badges/version/ps)](https://cran.r-project.org/package=ps)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/ps)](https://www.r-pkg.org/pkg/ps)
[![Coverage status](https://codecov.io/gh/r-lib/ps/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/ps?branch=master)

ps implements an API to query and manipulate system processes. Most of its
code is based on the [psutil](https://github.com/giampaolo/psutil) Python
package.

  - [Installation](#installation)
  - [Supported platforms](#supported-platforms)
  - [Listing all processes](#listing-all-processes)
  - [Process API](#process-api)
      - [Query functions](#query-functions)
      - [Process manipulation](#process-manipulation)
  - [Finished and zombie processes](#finished-and-zombie-processes)
  - [Pid reuse](#pid-reuse)
  - [Recipes](#recipes)
      - [Find process by name](#find-process-by-name)
      - [Wait for a process to finish](#wait-for-a-process-to-finish)
      - [Wait for several processes to
        finish](#wait-for-several-processes-to-finish)
      - [Kill process tree](#kill-process-tree)
      - [Terminate children](#terminate-children)
      - [Filtering and sorting
        processes](#filtering-and-sorting-processes)
  - [Contributions](#contributions)
  - [License](#license)

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

    ##  [1]  0  1 51 52 55 56 57 59 62 63 64 65 70 74 76 77 82 83 85 87

`ps()` returns a data frame (tibble if you have the tibble package
available), with data about each process. It contains a handle to each
process, in the `ps_handle` column, you can use these to perform more
queries on the processes.

``` r
ps()
```

    ## # A tibble: 386 x 11
    ##      pid  ppid name  username status     user  system     rss     vms created             ps_handle
    ##  * <int> <int> <chr> <chr>    <chr>     <dbl>   <dbl>   <dbl>   <dbl> <dttm>              <I(list)>
    ##  1 98737     1 quic… gaborcs… runni…   0.0425  0.0200  2.48e7  3.09e9 2018-07-24 09:41:40 <S3: ps_…
    ##  2 98327     1 mdwo… _spotli… runni…  NA      NA      NA      NA      2018-07-24 09:39:02 <S3: ps_…
    ##  3 98318  3718 Goog… gaborcs… runni…   0.0650  0.0276  4.62e7  3.36e9 2018-07-24 09:37:30 <S3: ps_…
    ##  4 96820  3718 Goog… gaborcs… runni…   0.114   0.0618  5.14e7  3.38e9 2018-07-24 09:10:29 <S3: ps_…
    ##  5 96817  3718 Goog… gaborcs… runni…   0.953   0.163   1.14e8  3.56e9 2018-07-24 09:09:42 <S3: ps_…
    ##  6 96816  3718 Goog… gaborcs… runni…   1.58    0.265   1.45e8  3.66e9 2018-07-24 09:09:37 <S3: ps_…
    ##  7 96809  3718 Goog… gaborcs… runni…   0.865   0.158   1.41e8  3.59e9 2018-07-24 09:09:34 <S3: ps_…
    ##  8 96680  3718 Goog… gaborcs… runni…  12.5     1.86    2.02e8  3.54e9 2018-07-24 08:41:27 <S3: ps_…
    ##  9 96679  3718 Goog… gaborcs… runni…  26.5     5.92    1.48e8  3.58e9 2018-07-24 08:41:27 <S3: ps_…
    ## 10 96678  3718 Goog… gaborcs… runni… 171.     16.1     2.72e8  3.70e9 2018-07-24 08:41:26 <S3: ps_…
    ## # ... with 376 more rows

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

    ## <ps::ps_handle> PID=93065, NAME=R, AT=2018-07-23 17:27:55

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 93065

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2018-07-23 17:27:55 GMT"

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

    ## [1] 90285

`ps_parent(p)` returns a process handle to the parent process of `p`.

``` r
ps_parent(p)
```

    ## <ps::ps_handle> PID=90285, NAME=zsh, AT=2018-07-23 16:15:32

`ps_name(p)` returns the name of the program `p` is running.

``` r
ps_name(p)
```

    ## [1] "R"

`ps_exe(p)` returns the full path to the executable the `p` is
    running.

``` r
ps_exe(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/3.5/Resources/bin/exec/R"

`ps_cmdline(p)` returns the command line (executable and arguments) of
`p`.

``` r
ps_cmdline(p)
```

    ## [1] "/Library/Frameworks/R.framework/Resources/bin/exec/R"

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

    ## [1] "/dev/ttys003"

`ps_environ(p)` returns the environment variables of the process.
`ps_environ_raw(p)` does the same, in a different form. Typically they
reflect the environment variables at the start of the process.

``` r
ps_environ(p)[c("TERM", "USER", "SHELL", "R_HOME")]
```

    ## TERM                          xterm-256color
    ## USER                          gaborcsardi
    ## SHELL                         /bin/zsh
    ## R_HOME                        /Library/Frameworks/R.framework/Resources

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

    ##            user          system    childen_user children_system 
    ##        8.023153        1.288586              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##        rss        vms    pfaults    pageins 
    ##  132501504 2719563776     318180       1028

`ps_children(p)` lists all child processes (potentially recuirsively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=90291, NAME=zsh, AT=2018-07-23 16:15:32
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=93065, NAME=R, AT=2018-07-23 17:27:55

`ps_num_fds(p)` returns the number of open file descriptors (handles on
Windows):

``` r
ps_num_fds(p)
```

    ## [1] 3

``` r
f <- file(tmp <- tempfile(), "w")
ps_num_fds(p)
```

    ## [1] 4

``` r
close(f)
unlink(tmp)
```

`ps_open_files(p)` lists all open files:

``` r
ps_open_files(p)
```

    ## # A tibble: 3 x 2
    ##      fd path        
    ##   <int> <chr>       
    ## 1     0 /dev/ttys003
    ## 2     1 /dev/ttys003
    ## 3     2 /dev/ttys003

``` r
f <- file(tmp <- tempfile(), "w")
ps_open_files(p)
```

    ## # A tibble: 4 x 2
    ##      fd path                                                                                 
    ##   <int> <chr>                                                                                
    ## 1     0 /dev/ttys003                                                                         
    ## 2     1 /dev/ttys003                                                                         
    ## 3     2 /dev/ttys003                                                                         
    ## 4     3 /private/var/folders/59/0gkmw1yj2w7bf2dfc3jznv5w0000gn/T/RtmpxkerNt/file16b892817efc1

``` r
close(f)
unlink(tmp)
ps_open_files(p)
```

    ## # A tibble: 3 x 2
    ##      fd path        
    ##   <int> <chr>       
    ## 1     0 /dev/ttys003
    ## 2     1 /dev/ttys003
    ## 3     2 /dev/ttys003

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
    ## <ps::ps_handle> PID=93065, NAME=R, AT=2018-07-23 17:27:55
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=86811, NAME=R, AT=2018-07-23 13:22:12
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=79811, NAME=R, AT=2018-07-23 12:15:12

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
    ## <ps::ps_handle> PID=79811, NAME=R, AT=2018-07-23 12:15:12
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=86811, NAME=R, AT=2018-07-23 13:22:12
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=93065, NAME=R, AT=2018-07-23 17:27:55

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
    ## <ps::ps_handle> PID=98990, NAME=???, AT=2018-07-24 09:45:30
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=98989, NAME=???, AT=2018-07-24 09:45:30
    ## 
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=98987, NAME=sleep, AT=2018-07-24 09:45:30
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=98988, NAME=sleep, AT=2018-07-24 09:45:30

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
    ## <ps::ps_handle> PID=98987, NAME=???, AT=2018-07-24 09:45:30
    ## 
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=98988, NAME=???, AT=2018-07-24 09:45:30
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=98991, NAME=???, AT=2018-07-24 09:45:32
    ## 
    ## $alive[[3]]
    ## <ps::ps_handle> PID=98992, NAME=???, AT=2018-07-24 09:45:32
    ## 
    ## $alive[[4]]
    ## <ps::ps_handle> PID=98993, NAME=???, AT=2018-07-24 09:45:32

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
    ## <ps::ps_handle> PID=98994, NAME=???, AT=2018-07-24 09:45:32
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=98995, NAME=???, AT=2018-07-24 09:45:32
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=98996, NAME=???, AT=2018-07-24 09:45:32
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

    ## # A tibble: 21 x 11
    ##      pid  ppid name  username  status     user  system    rss    vms created             ps_handle 
    ##    <int> <int> <chr> <chr>     <chr>     <dbl>   <dbl>  <dbl>  <dbl> <dttm>              <I(list)> 
    ##  1 94582 94576 zsh   gaborcsa… running 0.00595 0.00875 8.19e3 2.52e9 2018-07-23 21:06:28 <S3: ps_h…
    ##  2 94576 94575 zsh   gaborcsa… running 0.243   0.111   1.64e4 2.52e9 2018-07-23 21:06:28 <S3: ps_h…
    ##  3 93603 93597 zsh   gaborcsa… running 0.00659 0.00973 8.19e3 2.52e9 2018-07-23 17:44:33 <S3: ps_h…
    ##  4 93597 93596 zsh   gaborcsa… running 0.303   0.133   1.64e4 2.52e9 2018-07-23 17:44:33 <S3: ps_h…
    ##  5 93482 93476 zsh   gaborcsa… running 0.00524 0.00804 8.19e3 2.52e9 2018-07-23 17:40:47 <S3: ps_h…
    ##  6 93476 93475 zsh   gaborcsa… running 0.384   0.149   1.64e4 2.52e9 2018-07-23 17:40:46 <S3: ps_h…
    ##  7 90291 90285 zsh   gaborcsa… running 0.0118  0.0175  8.19e3 2.52e9 2018-07-23 16:15:32 <S3: ps_h…
    ##  8 90285 90284 zsh   gaborcsa… running 0.256   0.119   1.64e4 2.52e9 2018-07-23 16:15:32 <S3: ps_h…
    ##  9 90057 90051 zsh   gaborcsa… running 0.0290  0.0514  1.27e6 2.52e9 2018-07-23 16:14:26 <S3: ps_h…
    ## 10 90051 90050 zsh   gaborcsa… running 0.609   0.281   7.11e6 2.52e9 2018-07-23 16:14:26 <S3: ps_h…
    ## # ... with 11 more rows

Processes owned by user:

``` r
ps() %>%
  filter(username == Sys.info()[["user"]]) %>%
  select(pid, name)
```

    ## # A tibble: 258 x 2
    ##      pid name                
    ##    <int> <chr>               
    ##  1 98737 quicklookd          
    ##  2 98318 Google Chrome Helper
    ##  3 96820 Google Chrome Helper
    ##  4 96817 Google Chrome Helper
    ##  5 96816 Google Chrome Helper
    ##  6 96809 Google Chrome Helper
    ##  7 96680 Google Chrome Helper
    ##  8 96679 Google Chrome Helper
    ##  9 96678 Google Chrome Helper
    ## 10 96677 Google Chrome Helper
    ## # ... with 248 more rows

Processes consuming more than 100MB of memory:

``` r
ps() %>%
  filter(rss > 100 * 1024 * 1024)
```

    ## # A tibble: 16 x 11
    ##      pid  ppid name    username  status    user  system    rss    vms created             ps_handle
    ##    <int> <int> <chr>   <chr>     <chr>    <dbl>   <dbl>  <dbl>  <dbl> <dttm>              <I(list)>
    ##  1 96817  3718 Google… gaborcsa… runni… 9.53e-1 1.63e-1 1.14e8 3.56e9 2018-07-24 09:09:42 <S3: ps_…
    ##  2 96816  3718 Google… gaborcsa… runni… 1.58e+0 2.65e-1 1.45e8 3.66e9 2018-07-24 09:09:37 <S3: ps_…
    ##  3 96809  3718 Google… gaborcsa… runni… 8.65e-1 1.58e-1 1.41e8 3.59e9 2018-07-24 09:09:34 <S3: ps_…
    ##  4 96680  3718 Google… gaborcsa… runni… 1.25e+1 1.86e+0 2.02e8 3.54e9 2018-07-24 08:41:27 <S3: ps_…
    ##  5 96679  3718 Google… gaborcsa… runni… 2.65e+1 5.93e+0 1.48e8 3.58e9 2018-07-24 08:41:27 <S3: ps_…
    ##  6 96678  3718 Google… gaborcsa… runni… 1.71e+2 1.61e+1 2.72e8 3.70e9 2018-07-24 08:41:26 <S3: ps_…
    ##  7 96674  3718 Google… gaborcsa… runni… 1.04e+1 2.29e+0 1.79e8 3.63e9 2018-07-24 08:41:26 <S3: ps_…
    ##  8 96673  3718 Google… gaborcsa… runni… 1.06e+2 1.55e+1 1.71e8 3.62e9 2018-07-24 08:41:25 <S3: ps_…
    ##  9 95883 95882 Virtua… gaborcsa… runni… 2.03e+3 1.32e+3 4.73e9 7.53e9 2018-07-23 22:50:14 <S3: ps_…
    ## 10 93065 90285 R       gaborcsa… runni… 8.77e+0 1.53e+0 1.33e8 2.75e9 2018-07-23 17:27:55 <S3: ps_…
    ## 11 90173 90051 Emacs-… gaborcsa… runni… 3.18e+2 5.53e+1 1.99e8 2.89e9 2018-07-23 16:14:37 <S3: ps_…
    ## 12 92685  3718 Google… gaborcsa… runni… 1.20e+3 1.48e+2 5.32e8 4.89e9 2018-07-22 07:19:55 <S3: ps_…
    ## 13 35685  3718 Google… gaborcsa… runni… 7.01e+2 7.20e+1 3.67e8 4.15e9 2018-07-19 08:42:23 <S3: ps_…
    ## 14  3722  3718 Google… gaborcsa… runni… 2.84e+3 1.36e+3 1.44e8 3.40e9 2018-07-17 21:26:21 <S3: ps_…
    ## 15  3718     1 Google… gaborcsa… runni… 1.14e+4 3.85e+3 4.69e8 4.44e9 2018-07-17 21:26:21 <S3: ps_…
    ## 16   722     1 iTerm2  gaborcsa… runni… 6.36e+3 1.07e+3 2.24e8 3.59e9 2018-07-17 20:26:27 <S3: ps_…

Top 3 memory consuming processes:

``` r
ps() %>%
  top_n(3, rss) %>%
  arrange(desc(rss))
```

    ## # A tibble: 3 x 11
    ##     pid  ppid name     username  status    user system    rss    vms created             ps_handle 
    ##   <int> <int> <chr>    <chr>     <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dttm>              <I(list)> 
    ## 1 95883 95882 Virtual… gaborcsa… running  2034.  1320. 4.73e9 7.53e9 2018-07-23 22:50:14 <S3: ps_h…
    ## 2 92685  3718 Google … gaborcsa… running  1201.   148. 5.32e8 4.89e9 2018-07-22 07:19:55 <S3: ps_h…
    ## 3  3718     1 Google … gaborcsa… running 11431.  3848. 4.69e8 4.44e9 2018-07-17 21:26:21 <S3: ps_h…

Top 3 processes which consumed the most CPU time:

``` r
ps() %>%
  mutate(cpu_time = user + system) %>%
  top_n(3, cpu_time) %>%
  arrange(desc(cpu_time)) %>%
  select(pid, name, cpu_time)
```

    ## # A tibble: 3 x 3
    ##     pid name                       cpu_time
    ##   <int> <chr>                         <dbl>
    ## 1 40706 com.docker.hyperkit          31685.
    ## 2 38474 Google Chrome Helper (GPU)   23568.
    ## 3 38466 Google Chrome                20589.

## Contributions

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/r-lib/ps/blob/master/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## License

BSD © RStudio
