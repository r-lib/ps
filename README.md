
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
  - [Filtering and sorting processes](#filtering-and-sorting-processes)
- [Code of Conduct](#code-of-conduct)
- [License](#license)

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

    ##  [1]    0    1 1125 1127 1129 1130 1133 1136 1138 1139 1144 1149 1153 1155 1156 1161 1164 1165 1166
    ## [20] 1167

`ps()` returns a data frame, with data about each process. It contains a
handle to each process, in the `ps_handle` column, you can use these to
perform more queries on the processes.

``` r
ps()
```

    ## # A data frame: 477 × 11
    ##      pid  ppid name  username status    user  system     rss      vms created             ps_handle
    ##  * <int> <int> <chr> <chr>    <chr>    <dbl>   <dbl>   <dbl>    <dbl> <dttm>              <I<list>>
    ##  1 81068     1 mdwo… gaborcs… runni…  0.0377  0.0143  2.61e7  4.20e11 2024-09-01 09:39:46 <ps_handl>
    ##  2 81067     1 mdwo… gaborcs… runni…  0.0423  0.0133  2.19e7  4.20e11 2024-09-01 09:39:46 <ps_handl>
    ##  3 80413     1 mdwo… gaborcs… runni…  0.0717  0.0220  2.84e7  4.20e11 2024-09-01 09:39:27 <ps_handl>
    ##  4 80412     1 mdwo… gaborcs… runni…  0.0749  0.0241  3.00e7  4.20e11 2024-09-01 09:39:27 <ps_handl>
    ##  5 80366     1 mdwo… gaborcs… runni…  0.287   0.0797  3.00e7  4.20e11 2024-09-01 09:37:07 <ps_handl>
    ##  6 80360 69319 Goog… gaborcs… runni…  0.0462  0.0191  9.14e7  1.66e12 2024-09-01 09:37:05 <ps_handl>
    ##  7 80264     1 coun… root     runni… NA      NA      NA      NA       2024-09-01 09:36:10 <ps_handl>
    ##  8 80261     1 mdwo… gaborcs… runni…  0.300   0.0755  3.00e7  4.20e11 2024-09-01 09:35:55 <ps_handl>
    ##  9 78900 69319 Goog… gaborcs… runni…  2.26    0.429   1.76e8  1.66e12 2024-09-01 09:26:07 <ps_handl>
    ## 10 78888 69319 Goog… gaborcs… runni…  5.68    0.595   2.38e8  1.66e12 2024-09-01 09:25:57 <ps_handl>
    ## # ℹ 467 more rows

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

    ## <ps::ps_handle> PID=56773, NAME=R, AT=2024-08-31 14:11:26.708347

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 56773

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2024-08-31 14:11:26 GMT"

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

    ## [1] 55975

`ps_parent(p)` returns a process handle to the parent process of `p`.

``` r
ps_parent(p)
```

    ## <ps::ps_handle> PID=55975, NAME=zsh, AT=2024-08-29 15:04:35.020175

`ps_name(p)` returns the name of the program `p` is running.

``` r
ps_name(p)
```

    ## [1] "R"

`ps_exe(p)` returns the full path to the executable the `p` is running.

``` r
ps_exe(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/bin/exec/R"

`ps_cmdline(p)` returns the command line (executable and arguments) of
`p`.

``` r
ps_cmdline(p)
```

    ## [1] "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/bin/exec/R"

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

    ## [1] "/dev/ttys015"

`ps_environ(p)` returns the environment variables of the process.
`ps_environ_raw(p)` does the same, in a different form. Typically they
reflect the environment variables at the start of the process.

``` r
ps_environ(p)[c("TERM", "USER", "SHELL", "R_HOME")]
```

    ## TERM                          xterm-256color
    ## USER                          gaborcsardi
    ## SHELL                         /bin/zsh
    ## R_HOME                        /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources

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
    ##        33.87137        15.05281              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##          rss          vms      pfaults      pageins
    ##    255983616 423297335296      2173700          742

`ps_children(p)` lists all child processes (potentially recursively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=56773, NAME=R, AT=2024-08-31 14:11:26.708347
    ##
    ## [[2]]
    ## <ps::ps_handle> PID=57966, NAME=zsh, AT=2024-08-29 15:04:37.802087

`ps_num_fds(p)` returns the number of open file descriptors (handles on
Windows):

``` r
ps_num_fds(p)
```

    ## [1] 49

``` r
f <- file(tmp <- tempfile(), "w")
ps_num_fds(p)
```

    ## [1] 50

``` r
close(f)
unlink(tmp)
```

`ps_open_files(p)` lists all open files:

``` r
ps_open_files(p)
```

    ## # A data frame: 3 × 2
    ##       fd path
    ##    <int> <chr>
    ##  1     0 /dev/ttys015
    ##  2     1 /dev/ttys015
    ##  3     2 /dev/ttys015

``` r
f <- file(tmp <- tempfile(), "w")
ps_open_files(p)
```

    ## # A data frame: 4 × 2
    ##       fd path
    ##    <int> <chr>
    ##  1     0 /dev/ttys015
    ##  2     1 /dev/ttys015
    ##  3     2 /dev/ttys015
    ##  4    45 /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpFPtZXU/fileddc51cac4863

``` r
close(f)
unlink(tmp)
ps_open_files(p)
```

    ## # A data frame: 3 × 2
    ##       fd path
    ##    <int> <chr>
    ##  1     0 /dev/ttys015
    ##  2     1 /dev/ttys015
    ##  3     2 /dev/ttys015

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
    ## <ps::ps_handle> PID=56773, NAME=R, AT=2024-08-31 14:11:26.708347

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
    ## <ps::ps_handle> PID=56773, NAME=R, AT=2024-08-31 14:11:26.708347

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
ps() %>%
  filter(grepl("sh$", name))
```

    ## # A data frame: 38 × 11
    ##      pid  ppid name    username status    user  system    rss     vms created             ps_handle
    ##    <int> <int> <chr>   <chr>    <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>>
    ##  1 67374     1 Report… gaborcs… runni… 0.00899 0.0148  5.47e6 4.20e11 2024-09-01 03:02:35 <ps_handl>
    ##  2 44801 44603 zsh     gaborcs… runni… 0.00261 0.00945 7.86e5 4.21e11 2024-08-31 09:42:36 <ps_handl>
    ##  3 44603 44602 zsh     gaborcs… runni… 0.155   0.0547  9.99e5 4.21e11 2024-08-31 09:42:35 <ps_handl>
    ##  4 24830 24631 zsh     gaborcs… runni… 0.00736 0.0332  7.86e5 4.21e11 2024-08-30 22:11:24 <ps_handl>
    ##  5 24631 24630 zsh     gaborcs… runni… 0.333   0.134   9.67e5 4.21e11 2024-08-30 22:11:24 <ps_handl>
    ##  6 58680 55972 zsh     gaborcs… runni… 0.168   0.651   9.34e5 4.21e11 2024-08-29 15:04:39 <ps_handl>
    ##  7 58570 55971 zsh     gaborcs… runni… 0.0186  0.0693  2.38e6 4.21e11 2024-08-29 15:04:39 <ps_handl>
    ##  8 58509 55974 zsh     gaborcs… runni… 0.00364 0.0117  7.54e5 4.21e11 2024-08-29 15:04:38 <ps_handl>
    ##  9 58474 55964 zsh     gaborcs… runni… 0.00367 0.00942 7.54e5 4.21e11 2024-08-29 15:04:38 <ps_handl>
    ## 10 58437 55966 zsh     gaborcs… runni… 0.00341 0.00986 7.54e5 4.21e11 2024-08-29 15:04:38 <ps_handl>
    ## # ℹ 28 more rows

Processes owned by user:

``` r
ps() %>%
  filter(username == Sys.info()[["user"]]) %>%
  select(pid, name)
```

    ## # A data frame: 286 × 2
    ##      pid name
    ##    <int> <chr>
    ##  1 81199 Google Chrome Helper (Renderer)
    ##  2 81198 Google Chrome Helper (Renderer)
    ##  3 81197 Google Chrome Helper (Renderer)
    ##  4 81068 mdworker_shared
    ##  5 81067 mdworker_shared
    ##  6 80413 mdworker_shared
    ##  7 80412 mdworker_shared
    ##  8 80366 mdworker_shared
    ##  9 80360 Google Chrome Helper (Renderer)
    ## 10 80261 mdworker_shared
    ## # ℹ 276 more rows

Processes consuming more than 100MB of memory:

``` r
ps() %>%
  filter(rss > 100 * 1024 * 1024)
```

    ## # A data frame: 29 × 11
    ##      pid  ppid name    username status    user  system    rss     vms created             ps_handle
    ##    <int> <int> <chr>   <chr>    <chr>    <dbl>   <dbl>  <dbl>   <dbl> <dttm>              <I<list>>
    ##  1 81199 69319 Google… gaborcs… runni…  0.0412  0.0162 1.07e8 1.66e12 2024-09-01 09:39:54 <ps_handl>
    ##  2 81198 69319 Google… gaborcs… runni…  0.0704  0.0242 1.22e8 1.66e12 2024-09-01 09:39:53 <ps_handl>
    ##  3 81197 69319 Google… gaborcs… runni…  1.23    0.565  3.12e8 1.66e12 2024-09-01 09:39:53 <ps_handl>
    ##  4 78900 69319 Google… gaborcs… runni…  2.27    0.430  1.76e8 1.66e12 2024-09-01 09:26:07 <ps_handl>
    ##  5 78888 69319 Google… gaborcs… runni…  5.68    0.596  2.38e8 1.66e12 2024-09-01 09:25:57 <ps_handl>
    ##  6 76222 69319 Google… gaborcs… runni… 34.6     2.86   2.22e8 1.66e12 2024-09-01 09:17:46 <ps_handl>
    ##  7 74007 69319 Google… gaborcs… runni…  6.59    1.08   1.46e8 1.66e12 2024-09-01 08:37:02 <ps_handl>
    ##  8 73963 69319 Google… gaborcs… runni…  7.16    0.987  1.86e8 1.66e12 2024-09-01 08:34:37 <ps_handl>
    ##  9 73601 69319 Google… gaborcs… runni… 98.1    19.2    1.94e8 1.66e12 2024-09-01 07:54:27 <ps_handl>
    ## 10 69386 69319 Google… gaborcs… runni…  1.38    0.244  1.09e8 1.66e12 2024-09-01 07:06:19 <ps_handl>
    ## # ℹ 19 more rows

Top 3 memory consuming processes:

``` r
ps() %>%
  top_n(3, rss) %>%
  arrange(desc(rss))
```

    ## # A data frame: 3 × 11
    ##     pid  ppid name       username status   user system    rss     vms created             ps_handle
    ##   <int> <int> <chr>      <chr>    <chr>   <dbl>  <dbl>  <dbl>   <dbl> <dttm>              <I<list>>
    ## 1 64110     1 com.apple… gaborcs… runni… 68283. 7992.  7.39e8 4.29e11 2024-07-17 08:14:51 <ps_handl>
    ## 2 56754 56683 ark        gaborcs… runni…   813.  112.  5.66e8 4.22e11 2024-08-29 15:04:36 <ps_handl>
    ## 3 69319     1 Google Ch… gaborcs… runni…   280.   86.9 5.63e8 4.56e11 2024-09-01 07:06:06 <ps_handl>

Top 3 processes which consumed the most CPU time:

``` r
ps() %>%
  mutate(cpu_time = user + system) %>%
  top_n(3, cpu_time) %>%
  arrange(desc(cpu_time)) %>%
  select(pid, name, cpu_time)
```

    ## # A data frame: 3 × 3
    ##     pid name                                    cpu_time
    ##   <int> <chr>                                      <dbl>
    ## 1 64110 com.apple.Virtualization.VirtualMachine   76275.
    ## 2  3525 Dato                                      12825.
    ## 3  2617 CursorUIViewService                        6700.

## Code of Conduct

Please note that the ps project is released with a [Contributor Code of
Conduct](https://ps.r-lib.org/CODE_OF_CONDUCT.html). By contributing to
this project, you agree to abide by its terms.

## License

MIT © RStudio
