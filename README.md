
# ps

> List, Query, Manipulate System
Processes

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/r-lib/ps.svg?branch=master)](https://travis-ci.org/r-lib/ps)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/r-lib/ps?branch=master&svg=true)](https://ci.appveyor.com/project/gaborcsardi/ps)
[![CRAN
status](https://www.r-pkg.org/badges/version/ps)](https://cran.r-project.org/package=ps)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/ps)](https://www.r-pkg.org/pkg/ps)
[![Coverage
status](https://codecov.io/gh/r-lib/ps/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/ps?branch=master)

## Introduction

ps implement an API to query and manipulate system processes. Most of
its code is based on the [psutil](https://github.com/giampaolo/psutil)
Python package.

## Installation

Once released, you can install the released version of ps from
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

    ## # A tibble: 420 x 11
    ##      pid  ppid name   username status    user  system     rss     vms created             ps_handle
    ##  * <int> <int> <chr>  <chr>    <chr>    <dbl>   <dbl>   <dbl>   <dbl> <dttm>              <I(list)>
    ##  1 91883     1 quick… gaborcs… runni…  0.0897  0.0347  2.49e7  3.09e9 2018-07-21 22:56:54 <S3: ps_…
    ##  2 91856  3718 Googl… gaborcs… runni…  0.0741  0.0285  4.38e7  3.36e9 2018-07-21 22:55:55 <S3: ps_…
    ##  3 91662  3718 Googl… gaborcs… runni…  2.63    0.508   9.91e7  3.54e9 2018-07-21 22:54:53 <S3: ps_…
    ##  4 91190  3718 Googl… gaborcs… runni…  0.499   0.0986  1.05e8  3.55e9 2018-07-21 22:50:19 <S3: ps_…
    ##  5 91188  3718 Googl… gaborcs… runni…  1.69    0.406   1.22e8  3.47e9 2018-07-21 22:50:15 <S3: ps_…
    ##  6 91148  3718 Googl… gaborcs… runni…  1.25    0.219   9.79e7  3.54e9 2018-07-21 22:49:49 <S3: ps_…
    ##  7 90113     1 netbi… _netbios runni… NA      NA      NA      NA      2018-07-21 22:27:22 <S3: ps_…
    ##  8 89609     1 mdwor… gaborcs… runni…  1.08    0.231   8.39e7  2.58e9 2018-07-21 22:14:50 <S3: ps_…
    ##  9 89604     1 mdwor… gaborcs… runni…  1.82    0.277   8.46e7  2.58e9 2018-07-21 22:13:30 <S3: ps_…
    ## 10 89599     1 mdwor… gaborcs… runni…  1.67    0.264   8.25e7  2.58e9 2018-07-21 22:11:03 <S3: ps_…
    ## # ... with 410 more rows

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

    ## <ps::ps_handle> PID=88809, NAME=R, AT=2018-07-21 20:22:48

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 88809

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2018-07-21 20:22:48 GMT"

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

    ## [1] 42530

`ps_parent(p)` returns a process handle to the parent process of `p`.

``` r
ps_parent(p)
```

    ## <ps::ps_handle> PID=42530, NAME=zsh, AT=2018-07-19 16:34:33

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

    ## [1] "/dev/ttys024"

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

    ## [1] 2

`ps_cpu_times(p)` returns the CPU times of the process, similarly to
`proc.time()`.

``` r
ps_cpu_times(p)
```

    ##            user          system    childen_user children_system 
    ##       22.708576        4.744178              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##        rss        vms    pfaults    pageins 
    ##  171450368 2734481408     739502        260

`ps_children(p)` lists all child processes (potentially recuirsively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=42536, NAME=zsh, AT=2018-07-19 16:34:33
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=88809, NAME=R, AT=2018-07-21 20:22:48

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
    ## <ps::ps_handle> PID=88923, NAME=R, AT=2018-07-21 20:29:03
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=88809, NAME=R, AT=2018-07-21 20:22:48
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=86599, NAME=R, AT=2018-07-21 14:19:02
    ## 
    ## [[4]]
    ## <ps::ps_handle> PID=74578, NAME=R, AT=2018-07-20 13:36:22
    ## 
    ## [[5]]
    ## <ps::ps_handle> PID=32703, NAME=R, AT=2018-07-18 16:51:35
    ## 
    ## [[6]]
    ## <ps::ps_handle> PID=28777, NAME=R, AT=2018-07-18 08:30:28
    ## 
    ## [[7]]
    ## <ps::ps_handle> PID=24580, NAME=R, AT=2018-07-18 08:28:05

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
    ## <ps::ps_handle> PID=24580, NAME=R, AT=2018-07-18 08:28:05
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=28777, NAME=R, AT=2018-07-18 08:30:28
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=32703, NAME=R, AT=2018-07-18 16:51:35
    ## 
    ## [[4]]
    ## <ps::ps_handle> PID=74578, NAME=R, AT=2018-07-20 13:36:22
    ## 
    ## [[5]]
    ## <ps::ps_handle> PID=86599, NAME=R, AT=2018-07-21 14:19:02
    ## 
    ## [[6]]
    ## <ps::ps_handle> PID=88809, NAME=R, AT=2018-07-21 20:22:48
    ## 
    ## [[7]]
    ## <ps::ps_handle> PID=88923, NAME=R, AT=2018-07-21 20:29:03

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
    ## <ps::ps_handle> PID=91996, NAME=???, AT=2018-07-21 23:00:23
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=91995, NAME=???, AT=2018-07-21 23:00:23
    ## 
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=91993, NAME=sleep, AT=2018-07-21 23:00:23
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=91994, NAME=sleep, AT=2018-07-21 23:00:23

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
    ## <ps::ps_handle> PID=91993, NAME=???, AT=2018-07-21 23:00:23
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=91994, NAME=???, AT=2018-07-21 23:00:23
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=91997, NAME=???, AT=2018-07-21 23:00:25
    ## 
    ## $gone[[4]]
    ## <ps::ps_handle> PID=91998, NAME=???, AT=2018-07-21 23:00:25
    ## 
    ## $gone[[5]]
    ## <ps::ps_handle> PID=91999, NAME=???, AT=2018-07-21 23:00:25
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
reap_children<-function(timeout = 3) {
  procs<-ps_children(ps_handle())

  ## SIGTERM
  lapply(procs, ps_terminate)

  ga<-wait_for_processes(procs, timeout = timeout)

  ## SIGKILL to the survivers
  if (length(ga$alive)) lapply(ga$alive, ps_kill)

  ga2<-wait_for_processes(ga$alive, timeout = timeout)

  ## Some might still survive
  list(gone = c(ga$gone, ga2$gone), alive = ga2$alive)
}

pxs <- replicate(3, processx::process$new("sleep", "3"))
reap_children()
```

    ## $gone
    ## $gone[[1]]
    ## <ps::ps_handle> PID=92000, NAME=???, AT=2018-07-21 23:00:25
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=92001, NAME=???, AT=2018-07-21 23:00:25
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=92002, NAME=???, AT=2018-07-21 23:00:25
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

    ## # A tibble: 37 x 11
    ##      pid  ppid name  username  status     user  system    rss    vms created             ps_handle 
    ##    <int> <int> <chr> <chr>     <chr>     <dbl>   <dbl>  <dbl>  <dbl> <dttm>              <I(list)> 
    ##  1 74523 74516 zsh   gaborcsa… running 0.00409 0.00578 1.16e6 2.52e9 2018-07-20 13:36:18 <S3: ps_h…
    ##  2 74516 74515 zsh   gaborcsa… running 0.216   0.137   2.22e6 2.52e9 2018-07-20 13:36:17 <S3: ps_h…
    ##  3 42536 42530 zsh   gaborcsa… running 0.0388  0.0811  1.30e6 2.53e9 2018-07-19 16:34:33 <S3: ps_h…
    ##  4 42530 42529 zsh   gaborcsa… running 0.609   0.281   7.17e6 2.53e9 2018-07-19 16:34:33 <S3: ps_h…
    ##  5 41855 41849 zsh   gaborcsa… running 0.00936 0.0187  9.01e4 2.52e9 2018-07-19 16:30:40 <S3: ps_h…
    ##  6 41849 41848 zsh   gaborcsa… running 0.331   0.129   1.19e6 2.52e9 2018-07-19 16:30:40 <S3: ps_h…
    ##  7 37457 37451 zsh   gaborcsa… running 0.0182  0.0328  7.29e5 2.52e9 2018-07-19 12:58:12 <S3: ps_h…
    ##  8 37451 37450 zsh   gaborcsa… running 0.415   0.204   1.34e6 2.52e9 2018-07-19 12:58:12 <S3: ps_h…
    ##  9 37159 37153 zsh   gaborcsa… running 0.0282  0.0464  9.01e4 2.52e9 2018-07-19 12:57:39 <S3: ps_h…
    ## 10 37153 37152 zsh   gaborcsa… running 0.749   0.312   7.91e5 2.52e9 2018-07-19 12:57:38 <S3: ps_h…
    ## # ... with 27 more rows

Processes owned by user:

``` r
ps() %>%
  filter(username == Sys.info()[["user"]]) %>%
  select(pid, name)
```

    ## # A tibble: 294 x 2
    ##      pid name                
    ##    <int> <chr>               
    ##  1 91883 quicklookd          
    ##  2 91856 Google Chrome Helper
    ##  3 91662 Google Chrome Helper
    ##  4 91190 Google Chrome Helper
    ##  5 91188 Google Chrome Helper
    ##  6 91148 Google Chrome Helper
    ##  7 89609 mdworker            
    ##  8 89604 mdworker            
    ##  9 89599 mdworker            
    ## 10 89598 mdworker            
    ## # ... with 284 more rows

Processes consuming more than 100MB of memory:

``` r
ps() %>%
  filter(rss > 100 * 1024 * 1024)
```

    ## # A tibble: 9 x 11
    ##     pid  ppid name    username  status     user  system    rss    vms created             ps_handle
    ##   <int> <int> <chr>   <chr>     <chr>     <dbl>   <dbl>  <dbl>  <dbl> <dttm>              <I(list)>
    ## 1 91190  3718 Google… gaborcsa… running 4.99e-1 9.86e-2 1.05e8 3.55e9 2018-07-21 22:50:19 <S3: ps_…
    ## 2 91188  3718 Google… gaborcsa… running 1.70e+0 4.08e-1 1.22e8 3.47e9 2018-07-21 22:50:15 <S3: ps_…
    ## 3 88809 42530 R       gaborcsa… running 2.32e+1 4.96e+0 1.71e8 2.73e9 2018-07-21 20:22:48 <S3: ps_…
    ## 4 35685  3718 Google… gaborcsa… running 3.64e+2 3.63e+1 3.63e8 4.05e9 2018-07-19 08:42:23 <S3: ps_…
    ## 5 34055 33870 Emacs-… gaborcsa… running 1.27e+3 1.92e+2 2.65e8 3.11e9 2018-07-18 21:07:19 <S3: ps_…
    ## 6  3736  3718 Google… gaborcsa… running 1.97e+2 4.56e+1 1.07e8 3.49e9 2018-07-17 21:26:22 <S3: ps_…
    ## 7  3722  3718 Google… gaborcsa… running 1.56e+3 7.66e+2 3.40e8 3.42e9 2018-07-17 21:26:21 <S3: ps_…
    ## 8  3718     1 Google… gaborcsa… running 6.26e+3 2.14e+3 4.51e8 4.25e9 2018-07-17 21:26:21 <S3: ps_…
    ## 9   722     1 iTerm2  gaborcsa… running 4.75e+3 8.11e+2 5.48e8 3.69e9 2018-07-17 20:26:27 <S3: ps_…

Top 3 memory consuming processes:

``` r
ps() %>%
  top_n(3, rss) %>%
  arrange(desc(rss))
```

    ## # A tibble: 3 x 11
    ##     pid  ppid name      username  status   user system    rss    vms created             ps_handle 
    ##   <int> <int> <chr>     <chr>     <chr>   <dbl>  <dbl>  <dbl>  <dbl> <dttm>              <I(list)> 
    ## 1   722     1 iTerm2    gaborcsa… running 4754.  811.  5.48e8 3.69e9 2018-07-17 20:26:27 <S3: ps_h…
    ## 2  3718     1 Google C… gaborcsa… running 6264. 2140.  4.51e8 4.25e9 2018-07-17 21:26:21 <S3: ps_h…
    ## 3 35685  3718 Google C… gaborcsa… running  364.   36.3 3.63e8 4.05e9 2018-07-19 08:42:23 <S3: ps_h…

Top 3 processes which consumed the most CPU time:

``` r
ps() %>%
  mutate(cpu_time = user + system) %>%
  top_n(3, cpu_time) %>%
  arrange(desc(cpu_time))
```

    ## # A tibble: 3 x 12
    ##     pid  ppid name  username status  user system    rss    vms created             ps_handle
    ##   <int> <int> <chr> <chr>    <chr>  <dbl>  <dbl>  <dbl>  <dbl> <dttm>              <I(list)>
    ## 1  3718     1 Goog… gaborcs… runni… 6264.  2140. 4.51e8 4.25e9 2018-07-17 21:26:21 <S3: ps_…
    ## 2   722     1 iTer… gaborcs… runni… 4754.   811. 5.48e8 3.69e9 2018-07-17 20:26:27 <S3: ps_…
    ## 3  3722  3718 Goog… gaborcs… runni… 1561.   766. 3.40e8 3.42e9 2018-07-17 21:26:21 <S3: ps_…
    ## # ... with 1 more variable: cpu_time <dbl>

## Contributions

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.

## License

BSD © RStudio
