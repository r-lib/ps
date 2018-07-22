
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

    ## # A tibble: 418 x 11
    ##      pid  ppid name  username status     user   system     rss     vms created             ps_hand…
    ##  * <int> <int> <chr> <chr>    <chr>     <dbl>    <dbl>   <dbl>   <dbl> <dttm>              <I(list>
    ##  1 93898  3718 Goog… gaborcs… runni…  6.22e-2   0.0254  4.37e7  3.36e9 2018-07-22 07:52:40 <S3: ps…
    ##  2 93897     1 quic… gaborcs… runni…  6.55e-2   0.0267  2.51e7  3.09e9 2018-07-22 07:52:34 <S3: ps…
    ##  3 93387     1 com.… gaborcs… runni…  8.20e-1   0.181   2.19e7  2.56e9 2018-07-22 07:36:30 <S3: ps…
    ##  4 92686  3718 Goog… gaborcs… runni…  3.22e-1   0.0939  8.41e7  3.42e9 2018-07-22 07:19:55 <S3: ps…
    ##  5 92685  3718 Goog… gaborcs… runni…  3.63e+1   3.45    4.91e8  4.39e9 2018-07-22 07:19:55 <S3: ps…
    ##  6 92600  3718 Goog… gaborcs… runni…  6.45e+2  66.5     7.41e7  2.82e9 2018-07-22 06:42:57 <S3: ps…
    ##  7 92597  3718 Goog… gaborcs… runni…  1.10e+3 251.      5.88e8  4.14e9 2018-07-22 06:42:47 <S3: ps…
    ##  8 92509     1 ocspd root     runni… NA        NA      NA      NA      2018-07-22 06:40:38 <S3: ps…
    ##  9 92499     1 netb… _netbios runni… NA        NA      NA      NA      2018-07-22 05:03:29 <S3: ps…
    ## 10 92402  3718 Goog… gaborcs… runni…  2.08e+1   3.24    1.52e8  3.62e9 2018-07-21 23:02:27 <S3: ps…
    ## # ... with 408 more rows

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
    ##       28.674792        6.569125              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##        rss        vms    pfaults    pageins 
    ##  137334784 2735529984    1053253        419

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
    ## <ps::ps_handle> PID=93915, NAME=???, AT=2018-07-22 07:54:53
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=93914, NAME=???, AT=2018-07-22 07:54:53
    ## 
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=93912, NAME=sleep, AT=2018-07-22 07:54:53
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=93913, NAME=sleep, AT=2018-07-22 07:54:53

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
    ## <ps::ps_handle> PID=93912, NAME=???, AT=2018-07-22 07:54:53
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=93913, NAME=???, AT=2018-07-22 07:54:53
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=93916, NAME=???, AT=2018-07-22 07:54:55
    ## 
    ## $gone[[4]]
    ## <ps::ps_handle> PID=93917, NAME=???, AT=2018-07-22 07:54:55
    ## 
    ## $gone[[5]]
    ## <ps::ps_handle> PID=93918, NAME=???, AT=2018-07-22 07:54:55
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
    ## <ps::ps_handle> PID=93919, NAME=???, AT=2018-07-22 07:54:55
    ## 
    ## $gone[[2]]
    ## <ps::ps_handle> PID=93920, NAME=???, AT=2018-07-22 07:54:55
    ## 
    ## $gone[[3]]
    ## <ps::ps_handle> PID=93921, NAME=???, AT=2018-07-22 07:54:55
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
    ##  1 74523 74516 zsh   gaborcsa… running 0.00409 0.00578 1.09e6 2.52e9 2018-07-20 13:36:18 <S3: ps_h…
    ##  2 74516 74515 zsh   gaborcsa… running 0.216   0.137   2.15e6 2.52e9 2018-07-20 13:36:17 <S3: ps_h…
    ##  3 42536 42530 zsh   gaborcsa… running 0.0388  0.0811  6.51e5 2.53e9 2018-07-19 16:34:33 <S3: ps_h…
    ##  4 42530 42529 zsh   gaborcsa… running 0.609   0.281   1.53e6 2.53e9 2018-07-19 16:34:33 <S3: ps_h…
    ##  5 41855 41849 zsh   gaborcsa… running 0.00936 0.0187  1.23e4 2.52e9 2018-07-19 16:30:40 <S3: ps_h…
    ##  6 41849 41848 zsh   gaborcsa… running 0.331   0.129   6.96e5 2.52e9 2018-07-19 16:30:40 <S3: ps_h…
    ##  7 37457 37451 zsh   gaborcsa… running 0.0182  0.0328  6.55e5 2.52e9 2018-07-19 12:58:12 <S3: ps_h…
    ##  8 37451 37450 zsh   gaborcsa… running 0.415   0.204   1.26e6 2.52e9 2018-07-19 12:58:12 <S3: ps_h…
    ##  9 37159 37153 zsh   gaborcsa… running 0.0282  0.0464  1.23e4 2.52e9 2018-07-19 12:57:39 <S3: ps_h…
    ## 10 37153 37152 zsh   gaborcsa… running 0.749   0.312   6.96e5 2.52e9 2018-07-19 12:57:38 <S3: ps_h…
    ## # ... with 27 more rows

Processes owned by user:

``` r
ps() %>%
  filter(username == Sys.info()[["user"]]) %>%
  select(pid, name)
```

    ## # A tibble: 292 x 2
    ##      pid name                  
    ##    <int> <chr>                 
    ##  1 93898 Google Chrome Helper  
    ##  2 93897 quicklookd            
    ##  3 93387 com.apple.iCloudHelper
    ##  4 92686 Google Chrome Helper  
    ##  5 92685 Google Chrome Helper  
    ##  6 92600 Google Chrome Helper  
    ##  7 92597 Google Chrome Helper  
    ##  8 92402 Google Chrome Helper  
    ##  9 89609 mdworker              
    ## 10 89604 mdworker              
    ## # ... with 282 more rows

Processes consuming more than 100MB of memory:

``` r
ps() %>%
  filter(rss > 100 * 1024 * 1024)
```

    ## # A tibble: 9 x 11
    ##     pid  ppid name     username  status    user system    rss    vms created             ps_handle 
    ##   <int> <int> <chr>    <chr>     <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dttm>              <I(list)> 
    ## 1 92685  3718 Google … gaborcsa… running   36.3 3.45e0 4.91e8 4.38e9 2018-07-22 07:19:55 <S3: ps_h…
    ## 2 92597  3718 Google … gaborcsa… running 1096.  2.51e2 5.90e8 4.15e9 2018-07-22 06:42:47 <S3: ps_h…
    ## 3 92402  3718 Google … gaborcsa… running   20.8 3.24e0 1.52e8 3.62e9 2018-07-21 23:02:27 <S3: ps_h…
    ## 4 88809 42530 R        gaborcsa… running   29.2 6.80e0 1.39e8 2.74e9 2018-07-21 20:22:48 <S3: ps_h…
    ## 5 35685  3718 Google … gaborcsa… running  377.  3.79e1 3.15e8 4.05e9 2018-07-19 08:42:23 <S3: ps_h…
    ## 6 34055 33870 Emacs-x… gaborcsa… running 1284.  1.95e2 1.94e8 3.10e9 2018-07-18 21:07:19 <S3: ps_h…
    ## 7  3722  3718 Google … gaborcsa… running 1719.  8.28e2 2.79e8 3.51e9 2018-07-17 21:26:21 <S3: ps_h…
    ## 8  3718     1 Google … gaborcsa… running 7037.  2.40e3 4.09e8 4.35e9 2018-07-17 21:26:21 <S3: ps_h…
    ## 9   722     1 iTerm2   gaborcsa… running 4827.  8.22e2 4.85e8 3.68e9 2018-07-17 20:26:27 <S3: ps_h…

Top 3 memory consuming processes:

``` r
ps() %>%
  top_n(3, rss) %>%
  arrange(desc(rss))
```

    ## # A tibble: 3 x 11
    ##     pid  ppid name     username  status    user system    rss    vms created             ps_handle 
    ##   <int> <int> <chr>    <chr>     <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dttm>              <I(list)> 
    ## 1 92597  3718 Google … gaborcsa… running 1096.  252.   5.90e8 4.15e9 2018-07-22 06:42:47 <S3: ps_h…
    ## 2 92685  3718 Google … gaborcsa… running   36.3   3.45 4.91e8 4.38e9 2018-07-22 07:19:55 <S3: ps_h…
    ## 3   722     1 iTerm2   gaborcsa… running 4827.  822.   4.85e8 3.68e9 2018-07-17 20:26:27 <S3: ps_h…

Top 3 processes which consumed the most CPU time:

``` r
ps() %>%
  mutate(cpu_time = user + system) %>%
  top_n(3, cpu_time) %>%
  arrange(desc(cpu_time)) %>%
  select(pid, name, cpu_time)
```

    ## # A tibble: 3 x 3
    ##     pid name                 cpu_time
    ##   <int> <chr>                   <dbl>
    ## 1  3718 Google Chrome           9442.
    ## 2   722 iTerm2                  5649.
    ## 3  3722 Google Chrome Helper    2547.

## Contributions

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.

## License

BSD © RStudio
