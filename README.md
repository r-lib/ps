
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

`ps()` is similar, but it returns a data frame (tibble if you have the
tibble package available), with data about each process:

``` r
ps()
```

    ## # A tibble: 428 x 11
    ##      pid  ppid name            username   status     user  system       rss        vms created             ps_handle   
    ##  * <int> <int> <chr>           <chr>      <chr>     <dbl>   <dbl>     <dbl>      <dbl> <dttm>              <I(list)>   
    ##  1 82532  3718 Google Chrome … gaborcsar… running  0.179   0.0403  71663616    3.40e 9 2018-07-21 08:01:02 <S3: ps_han…
    ##  2 82505  3718 Google Chrome … gaborcsar… running  0.0691  0.0252  43872256    3.36e 9 2018-07-21 08:00:12 <S3: ps_han…
    ##  3 82302  3718 Google Chrome … gaborcsar… running  4.80    0.904  113180672    3.55e 9 2018-07-21 07:59:25 <S3: ps_han…
    ##  4 82301     1 quicklookd      gaborcsar… running  0.0822  0.0311  25055232    3.09e 9 2018-07-21 07:59:16 <S3: ps_han…
    ##  5 82232 42530 R               gaborcsar… running  1.51    0.197  124272640    2.73e 9 2018-07-21 07:58:48 <S3: ps_han…
    ##  6 81150     1 applessdstatis… root       running NA      NA             NA   NA       2018-07-21 07:36:08 <S3: ps_han…
    ##  7 80990     1 ocspd           root       running NA      NA             NA   NA       2018-07-21 07:14:52 <S3: ps_han…
    ##  8 80975     1 netbiosd        _netbios   running NA      NA             NA   NA       2018-07-21 03:03:28 <S3: ps_han…
    ##  9 80936     1 aslmanager      root       running NA      NA             NA   NA       2018-07-21 03:03:22 <S3: ps_han…
    ## 10 80886 37083 docker          gaborcsar… running  0.270   0.147    8130560    5.70e11 2018-07-20 22:14:34 <S3: ps_han…
    ## # ... with 418 more rows

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

    ## <ps::ps_handle> PID=82232, NAME=R, AT=2018-07-21 07:58:48

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 82232

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2018-07-21 07:58:48 GMT"

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

    ## TERM                                xterm-256color
    ## USER                                gaborcsardi
    ## SHELL                               /bin/zsh
    ## R_HOME                              /Library/Frameworks/R.framework/Resources

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
    ##       1.6817899       0.2100831              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##        rss        vms    pfaults    pageins 
    ##  125579264 2696826880      62429          2

`ps_children(p)` lists all child processes (potentially recuirsively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=42536, NAME=zsh, AT=2018-07-19 16:34:33
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=82232, NAME=R, AT=2018-07-21 07:58:48

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

## Recipes:

In the spirit of [psutil
recipes](http://psutil.readthedocs.io/en/latest/#recipes).

### Find process by name

Using `ps()` and dplyr:

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
find_procs_by_name <- function(name) {
  ps() %>%
    filter(name == !!name)  %>%
    pull(ps_handle)
}

find_procs_by_name("R")
```

    ## [[1]]
    ## <ps::ps_handle> PID=82232, NAME=R, AT=2018-07-21 07:58:48
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=74578, NAME=R, AT=2018-07-20 13:36:22
    ## 
    ## [[3]]
    ## <ps::ps_handle> PID=60390, NAME=R, AT=2018-07-20 11:03:10
    ## 
    ## [[4]]
    ## <ps::ps_handle> PID=32703, NAME=R, AT=2018-07-18 16:51:35
    ## 
    ## [[5]]
    ## <ps::ps_handle> PID=28777, NAME=R, AT=2018-07-18 08:30:28
    ## 
    ## [[6]]
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
    ## <ps::ps_handle> PID=60390, NAME=R, AT=2018-07-20 11:03:10
    ## 
    ## [[5]]
    ## <ps::ps_handle> PID=74578, NAME=R, AT=2018-07-20 13:36:22
    ## 
    ## [[6]]
    ## <ps::ps_handle> PID=82232, NAME=R, AT=2018-07-21 07:58:48

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
  running <- vapply(children, ps_is_running, logical(1))
  list(gone = children[!running], alive = children[running])
}

p1 <- processx::process$new("sleep", "10")
p2 <- processx::process$new("sleep", "10")
p3 <- processx::process$new("sleep", "10")
kill_proc_tree(Sys.getpid(), include_parent = FALSE)
```

    ## $gone
    ## list()
    ## 
    ## $alive
    ## $alive[[1]]
    ## <ps::ps_handle> PID=82535, NAME=???, AT=2018-07-21 08:01:13
    ## 
    ## $alive[[2]]
    ## <ps::ps_handle> PID=82536, NAME=???, AT=2018-07-21 08:01:13
    ## 
    ## $alive[[3]]
    ## <ps::ps_handle> PID=82537, NAME=???, AT=2018-07-21 08:01:13

## Contributions

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.

## License

BSD © RStudio
