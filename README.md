
# ps

> List, Query, Manipulate System Processes

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

    ## # A tibble: 400 x 10
    ##      pid  ppid name                 username    status     user  system       rss        vms created            
    ##  * <int> <int> <chr>                <chr>       <chr>     <dbl>   <dbl>     <dbl>      <dbl> <dttm>             
    ##  1 55610     1 quicklookd           gaborcsardi running  0.0894  0.0334  25034752 3088658432 2018-07-20 10:02:08
    ##  2 55250  3718 Google Chrome Helper gaborcsardi running  0.0655  0.0254  43905024 3355127808 2018-07-20 10:00:41
    ##  3 55249  3718 Google Chrome Helper gaborcsardi running  0.317   0.0708  80433152 3402051584 2018-07-20 10:00:34
    ##  4 55247  3718 Google Chrome Helper gaborcsardi running  0.600   0.117  103858176 3449085952 2018-07-20 10:00:18
    ##  5 55087 42530 R                    gaborcsardi running  2.26    0.564  144371712 2751770624 2018-07-20 09:59:30
    ##  6 54757  3718 Google Chrome Helper gaborcsardi running  1.44    0.299   76734464 3519508480 2018-07-20 09:56:11
    ##  7 54525     1 mdworker             _spotlight  running NA      NA             NA         NA 2018-07-20 08:46:20
    ##  8 54358  3718 Google Chrome Helper gaborcsardi running  0.712   0.200   94986240 3426856960 2018-07-20 07:49:24
    ##  9 54347     1 netbiosd             _netbios    running NA      NA             NA         NA 2018-07-20 07:48:26
    ## 10 54288     1 ocspd                root        running NA      NA             NA         NA 2018-07-20 05:19:14
    ## # ... with 390 more rows

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

    ## <ps::ps_handle> PID=55087, NAME=R, AT=2018-07-20 09:59:30

### Query functions

`ps_pid(p)` returns the pid of the process.

``` r
ps_pid(p)
```

    ## [1] 55087

`ps_create_time()` returns the creation time of the process (according
to the OS).

``` r
ps_create_time(p)
```

    ## [1] "2018-07-20 09:59:30 GMT"

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
    ##       2.4006976       0.5763375              NA              NA

`ps_memory_info(p)` returns memory usage information. See the manual for
details.

``` r
ps_memory_info(p)
```

    ##        rss        vms    pfaults    pageins 
    ##  146587648 2722009088     184244        241

`ps_children(p)` lists all child processes (potentially recuirsively) of
the current process.

``` r
ps_children(ps_parent(p))
```

    ## [[1]]
    ## <ps::ps_handle> PID=42536, NAME=zsh, AT=2018-07-19 16:34:33
    ## 
    ## [[2]]
    ## <ps::ps_handle> PID=55087, NAME=R, AT=2018-07-20 09:59:30

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

## Contributions

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.

## License

BSD © RStudio
