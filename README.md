
# ps

> List, Query, Manipulate System Processes

<!-- badges: start -->
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/r-lib/ps/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/ps/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/ps)](https://cran.r-project.org/package=ps)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/ps)](https://www.r-pkg.org/pkg/ps)
[![Codecov test coverage](https://codecov.io/gh/r-lib/ps/graph/badge.svg)](https://app.codecov.io/gh/r-lib/ps)
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

If you need the development version, install it with

``` r
pak::pak("r-lib/ps")
```

``` r
library(ps)
library(pillar) # nicer printing of data frames
```

## Supported platforms

ps currently supports Windows (from Vista), macOS and Linux systems. On
unsupported platforms the package can be installed and loaded, but all
of its functions fail with an error of class `"not_implemented"`.

## Listing all processes

`ps_pids()` returns all process ids on the system. This can be useful to
iterate over all processes.

``` r
ps_pids()[1:20]
```

    ##  [1]   0   1 265 266 275 276 557 559 561 562 564 567 569 570 574 578 580 584 586 587

`ps()` returns a data frame, with data about each process. It contains a
handle to each process, in the `ps_handle` column, you can use these to
perform more queries on the processes.

``` r
ps()
```

    ## # A data frame: 572 × 11
    ##      pid  ppid name      username status   user system    rss     vms created             ps_handle 
    ##  * <int> <int> <chr>     <chr>    <chr>   <dbl>  <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 45911  4159 Google C… gaborcs… sleep… 0.0473 0.0227 1.05e8 1.91e12 2025-04-28 08:05:25 <ps_handl>
    ##  2 45860  4159 Google C… gaborcs… sleep… 0.0751 0.0347 1.21e8 1.91e12 2025-04-28 08:05:19 <ps_handl>
    ##  3 45700     1 mdworker… gaborcs… sleep… 0.327  0.0861 2.88e7 4.37e11 2025-04-28 08:04:26 <ps_handl>
    ##  4 44844 39673 R         gaborcs… runni… 1.70   0.634  2.55e8 4.22e11 2025-04-28 08:01:34 <ps_handl>
    ##  5 44738  4159 Google C… gaborcs… sleep… 0.0325 0.0157 4.77e7 4.89e11 2025-04-28 08:00:51 <ps_handl>
    ##  6 43786     1 Messages… gaborcs… sleep… 0.162  0.233  6.90e6 4.37e11 2025-04-28 07:59:53 <ps_handl>
    ##  7 43785     1 business… gaborcs… sleep… 0.110  0.112  8.72e6 4.37e11 2025-04-28 07:59:53 <ps_handl>
    ##  8 43628     1 MENotifi… gaborcs… sleep… 0.0291 0.0116 3.08e6 4.21e11 2025-04-28 07:59:35 <ps_handl>
    ##  9 43627     1 MTLAsset… gaborcs… sleep… 0.0498 0.0208 3.29e6 4.37e11 2025-04-28 07:59:35 <ps_handl>
    ## 10 43472     1 replayd   gaborcs… sleep… 2.32   0.560  2.46e7 4.37e11 2025-04-28 07:59:21 <ps_handl>
    ## # ℹ 562 more rows

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

    ## <ps::ps_handle> PID=44844, NAME=R, AT=2025-04-28 08:01:34.873763

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 44844

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2025-04-28 08:01:34 GMT"

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

    ## [1] 39673

`ps_parent(p)` returns a process handle to the parent process of `p`.

``` r
ps_parent(p)
```

    ## <ps::ps_handle> PID=39673, NAME=zsh, AT=2025-04-28 07:53:00.030062

`ps_name(p)` returns the name of the program `p` is running.

``` r
ps_name(p)
```

    ## [1] "R"

`ps_exe(p)` returns the full path to the executable the `p` is running.

``` r
ps_exe(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/bin/exec/R"

`ps_cmdline(p)` returns the command line (executable and arguments) of
`p`.

``` r
ps_cmdline(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/bin/exec/R"

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

    ## [1] "/dev/ttys019"

`ps_environ(p)` returns the environment variables of the process.
`ps_environ_raw(p)` does the same, in a different form. Typically they
reflect the environment variables at the start of the process.

``` r
ps_environ(p)[c("TERM", "USER", "SHELL", "R_HOME")]
```

    ## TERM                          xterm-256color
    ## USER                          gaborcsardi
    ## SHELL                         /bin/zsh
    ## R_HOME                        /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources

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
    ##       1.7681955       0.6514954              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##          rss          vms      pfaults      pageins 
    ##    261783552 421933465600        41422           13

`ps_children(p)` lists all child processes (potentially recursively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=39874, NAME=zsh, AT=2025-04-28 07:53:00.889347
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=44844, NAME=R, AT=2025-04-28 08:01:34.873763

`ps_num_fds(p)` returns the number of open file descriptors (handles on
Windows):

``` r
ps_num_fds(p)
```

    ## [1] 23

``` r
f <- file(tmp <- tempfile(), "w")
ps_num_fds(p)
```

    ## [1] 24

``` r
close(f)
unlink(tmp)
```

`ps_open_files(p)` lists all open files:

``` r
ps_open_files(p)
```

    ## # A data frame: 4 × 2
    ##      fd path                                                                              
    ##   <int> <chr>                                                                             
    ## 1     0 /dev/ttys019                                                                      
    ## 2     1 /dev/ttys019                                                                      
    ## 3     2 /dev/ttys019                                                                      
    ## 4    14 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpoaXwPG/Rfaf2c3705b93c

``` r
f <- file(tmp <- tempfile(), "w")
ps_open_files(p)
```

    ## # A data frame: 5 × 2
    ##      fd path                                                                                
    ##   <int> <chr>                                                                               
    ## 1     0 /dev/ttys019                                                                        
    ## 2     1 /dev/ttys019                                                                        
    ## 3     2 /dev/ttys019                                                                        
    ## 4    14 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpoaXwPG/Rfaf2c3705b93c  
    ## 5    23 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpoaXwPG/fileaf2c58c42e5c

``` r
close(f)
unlink(tmp)
ps_open_files(p)
```

    ## # A data frame: 4 × 2
    ##      fd path                                                                              
    ##   <int> <chr>                                                                             
    ## 1     0 /dev/ttys019                                                                      
    ## 2     1 /dev/ttys019                                                                      
    ## 3     2 /dev/ttys019                                                                      
    ## 4    14 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpoaXwPG/Rfaf2c3705b93c

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
  ps() |>
    filter(name == !!name)  |>
    pull(ps_handle)
}

find_procs_by_name("R")
```

    ## [[1]]
    ## <ps::ps_handle> PID=44844, NAME=R, AT=2025-04-28 08:01:34.873763
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=32722, NAME=R, AT=2025-04-28 07:36:42.295781

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
    ## <ps::ps_handle> PID=32722, NAME=R, AT=2025-04-28 07:36:42.295781
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=44844, NAME=R, AT=2025-04-28 08:01:34.873763

### Wait for a process to finish

`ps_wait()`, from ps 1.8.0, implements a new way, efficient for waiting
on a list of processes, so this is now very easy:

``` r
px <- processx::process$new("sleep", "2")
p <- px$as_ps_handle()
ps_wait(p, 1000)
```

    ## [1] FALSE

``` r
ps_wait(p)
```

    ## [1] TRUE

### Wait for several processes to finish

Again, this is much simpler with `ps_wait()`, added in ps 1.8.0.

``` r
px1 <- processx::process$new("sleep", "10")
px2 <- processx::process$new("sleep", "10")
px3 <- processx::process$new("sleep", "1")
px4 <- processx::process$new("sleep", "1")

p1 <- px1$as_ps_handle()
p2 <- px2$as_ps_handle()
p3 <- px3$as_ps_handle()
p4 <- px4$as_ps_handle()

ps_wait(list(p1, p2, p3, p4), timeout = 2000)
```

    ## [1] FALSE FALSE  TRUE  TRUE

### Kill process tree

From ps 1.8.0, `ps_kill()` will first send `SIGTERM` signals on Unix,
and `SIGKILL` after a grace period, if needed.

Note, that some R IDEs, including RStudio, run a multithreaded R
process, and other threads may start processes as well.
`reap_children()` will clean up all these as well, potentially causing
the IDE to misbehave or crash.

``` r
kill_proc_tree <- function(pid, include_parent = TRUE, ...) {
  if (pid == Sys.getpid() && include_parent) stop("I refuse to kill myself")
  parent <- ps_handle(pid)
  children <- ps_children(parent, recursive = TRUE)
  if (include_parent) children <- c(children, list(parent))
  ps_kill(children, ...)
}

p1 <- processx::process$new("sleep", "10")
p2 <- processx::process$new("sleep", "10")
p3 <- processx::process$new("sleep", "10")
kill_proc_tree(Sys.getpid(), include_parent = FALSE)
```

    ## [1] "terminated" "terminated" "terminated" "terminated" "terminated"

### Filtering and sorting processes

Process name ending with “sh”:

``` r
ps() |>
  filter(grepl("sh$", name))
```

    ## # A data frame: 35 × 11
    ##      pid  ppid name        username    status      user  system     rss      vms created            
    ##    <int> <int> <chr>       <chr>       <chr>      <dbl>   <dbl>   <dbl>    <dbl> <dttm>             
    ##  1 41838     1 ReportCrash root        sleepi… NA       NA      NA      NA       2025-04-28 07:57:52
    ##  2 40290 40090 zsh         gaborcsardi sleepi…  0.0361   0.154   3.01e6  4.21e11 2025-04-28 07:53:47
    ##  3 40090 40088 zsh         gaborcsardi sleepi…  0.587    0.406   3.14e7  4.21e11 2025-04-28 07:53:47
    ##  4 39874 39673 zsh         gaborcsardi sleepi…  0.00552  0.0208  2.62e6  4.21e11 2025-04-28 07:53:00
    ##  5 39673 39672 zsh         gaborcsardi sleepi…  0.270    0.0922  2.96e7  4.21e11 2025-04-28 07:53:00
    ##  6 35995 35795 zsh         gaborcsardi sleepi…  0.0117   0.0485  9.67e5  4.21e11 2025-04-28 07:45:03
    ##  7 35795 35794 zsh         gaborcsardi sleepi…  0.236    0.128   1.69e6  4.21e11 2025-04-28 07:45:03
    ##  8 35595 35391 zsh         gaborcsardi sleepi…  0.00781  0.0332  9.67e5  4.21e11 2025-04-28 07:44:16
    ##  9 35391 35390 zsh         gaborcsardi sleepi…  0.262    0.115   1.69e6  4.21e11 2025-04-28 07:44:15
    ## 10 30303 30302 bash        gaborcsardi sleepi…  0.00806  0.0194  8.68e5  4.20e11 2025-04-28 07:32:12
    ## # ℹ 25 more rows
    ## # ℹ 1 more variable: ps_handle <I<list>>

Processes owned by user:

``` r
ps() |>
  filter(username == Sys.info()[["user"]]) |>
  select(pid, name)
```

    ## # A data frame: 347 × 2
    ##      pid name                           
    ##    <int> <chr>                          
    ##  1 45911 Google Chrome Helper (Renderer)
    ##  2 45860 Google Chrome Helper (Renderer)
    ##  3 45700 mdworker_shared                
    ##  4 44844 R                              
    ##  5 44738 Google Chrome Helper           
    ##  6 43786 MessagesBlastDoorService       
    ##  7 43785 businessservicesd              
    ##  8 43628 MENotificationAgent            
    ##  9 43627 MTLAssetUpgraderD              
    ## 10 43472 replayd                        
    ## # ℹ 337 more rows

Processes consuming more than 100MB of memory:

``` r
ps() |>
  filter(rss > 100 * 1024 * 1024)
```

    ## # A data frame: 20 × 11
    ##      pid  ppid name    username status    user  system    rss     vms created             ps_handle 
    ##    <int> <int> <chr>   <chr>    <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ##  1 45911  4159 Google… gaborcs… sleep… 4.73e-2 2.27e-2 1.05e8 1.91e12 2025-04-28 08:05:25 <ps_handl>
    ##  2 45860  4159 Google… gaborcs… sleep… 7.51e-2 3.47e-2 1.21e8 1.91e12 2025-04-28 08:05:19 <ps_handl>
    ##  3 44844 39673 R       gaborcs… runni… 2.08e+0 9.20e-1 2.80e8 4.22e11 2025-04-28 08:01:34 <ps_handl>
    ##  4 40521  4159 Google… gaborcs… sleep… 3.14e+0 4.45e-1 2.03e8 1.91e12 2025-04-28 07:54:07 <ps_handl>
    ##  5 40037  4159 Google… gaborcs… sleep… 7.78e+0 1.03e+0 2.66e8 1.91e12 2025-04-28 07:53:23 <ps_handl>
    ##  6 39666  4159 Google… gaborcs… sleep… 1.30e+0 2.37e-1 1.45e8 1.91e12 2025-04-28 07:52:34 <ps_handl>
    ##  7 28176  4159 Google… gaborcs… sleep… 1.16e+1 1.09e+0 2.18e8 1.91e12 2025-04-28 07:23:37 <ps_handl>
    ##  8 18695  4159 Google… gaborcs… sleep… 2.33e+1 2.18e+0 2.23e8 1.91e12 2025-04-28 06:48:50 <ps_handl>
    ##  9 18673  4159 Google… gaborcs… sleep… 2.08e+1 1.73e+0 2.39e8 1.91e12 2025-04-28 06:47:44 <ps_handl>
    ## 10 18257  4159 Google… gaborcs… sleep… 2.11e+1 2.85e+0 2.02e8 1.91e12 2025-04-28 06:31:40 <ps_handl>
    ## 11 17380  4159 Google… gaborcs… sleep… 1.25e+1 1.50e+0 1.76e8 1.91e12 2025-04-28 05:48:24 <ps_handl>
    ## 12 16963  4159 Google… gaborcs… sleep… 4.73e+2 6.83e+1 5.52e8 1.91e12 2025-04-28 05:31:43 <ps_handl>
    ## 13  4214  4159 Google… gaborcs… sleep… 4.88e+0 1.34e+0 1.34e8 1.91e12 2025-04-26 23:27:03 <ps_handl>
    ## 14  4199  4159 Google… gaborcs… sleep… 1.26e+2 1.08e+1 3.45e8 1.91e12 2025-04-26 23:26:59 <ps_handl>
    ## 15  4176  4159 Google… gaborcs… sleep… 2.38e+2 2.31e+2 1.23e8 4.55e11 2025-04-26 23:26:58 <ps_handl>
    ## 16  4175  4159 Google… gaborcs… sleep… 2.05e+3 1.09e+3 1.35e8 4.56e11 2025-04-26 23:26:58 <ps_handl>
    ## 17  4159     1 Google… gaborcs… sleep… 1.21e+3 3.79e+2 6.00e8 4.56e11 2025-04-26 23:26:54 <ps_handl>
    ## 18 22365 22350 qemu-s… gaborcs… sleep… 1.26e+4 1.65e+3 1.58e8 4.26e11 2025-04-24 10:02:15 <ps_handl>
    ## 19  1726     1 Spotli… gaborcs… sleep… 1.64e+2 4.42e+1 1.47e8 4.25e11 2025-04-22 14:34:30 <ps_handl>
    ## 20  1644     1 iTerm2  gaborcs… sleep… 6.32e+3 1.32e+3 3.87e8 4.23e11 2025-04-22 14:34:22 <ps_handl>

Top 3 memory consuming processes:

``` r
ps() |>
  top_n(3, rss) |>
  arrange(desc(rss))
```

    ## # A data frame: 3 × 11
    ##     pid  ppid name        username status  user system    rss     vms created             ps_handle 
    ##   <int> <int> <chr>       <chr>    <chr>  <dbl>  <dbl>  <dbl>   <dbl> <dttm>              <I<list>> 
    ## 1  4159     1 Google Chr… gaborcs… sleep… 1211.  379.  6.00e8 4.56e11 2025-04-26 23:26:54 <ps_handl>
    ## 2 16963  4159 Google Chr… gaborcs… sleep…  473.   68.3 5.52e8 1.91e12 2025-04-28 05:31:43 <ps_handl>
    ## 3  1644     1 iTerm2      gaborcs… sleep… 6316. 1322.  3.87e8 4.23e11 2025-04-22 14:34:22 <ps_handl>

Top 3 processes which consumed the most CPU time:

``` r
ps() |>
  mutate(cpu_time = user + system) |>
  top_n(3, cpu_time) |>
  arrange(desc(cpu_time)) |>
  select(pid, name, cpu_time)
```

    ## # A data frame: 3 × 3
    ##     pid name                       cpu_time
    ##   <int> <chr>                         <dbl>
    ## 1 22365 qemu-system-aarch64          14269.
    ## 2  1644 iTerm2                        7639.
    ## 3  4175 Google Chrome Helper (GPU)    3140.

## Code of Conduct

Please note that the ps project is released with a [Contributor Code of
Conduct](https://ps.r-lib.org/CODE_OF_CONDUCT.html). By contributing to
this project, you agree to abide by its terms.

## License

MIT © RStudio
