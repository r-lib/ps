# Environment variables of a process

`ps_environ()` returns the environment variables of the process, in a
named vector, similarly to the return value of
[`Sys.getenv()`](https://rdrr.io/r/base/Sys.getenv.html) (without
arguments).

## Usage

``` r
ps_environ(p = ps_handle())

ps_environ_raw(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

`ps_environ()` returns a named character vector (that has a `Dlist`
class, so it is printed nicely), `ps_environ_raw()` returns a character
vector.

## Details

Note: this usually does not reflect changes made after the process
started.

`ps_environ_raw()` is similar to `p$environ()` but returns the unparsed
`"var=value"` strings. This is faster, and sometimes good enough.

These functions throw a `zombie_process` error for zombie processes.

## macOS issues

`ps_environ()` usually does not work on macOS nowadays. This is because
macOS does not allow reading the environment variables of another
process. Accoding to the Darwin source code, `ps_environ` will work is
one of these conditions hold:

- You are running a development or debug kernel, i.e. if you are
  debugging the macOS kernel itself.

- The target process is same as the calling process.

- SIP if off.

- The target process is not restricted, e.g. it is running a binary that
  was not signed.

- The calling process has the
  `com.apple.private.read-environment-variables` entitlement. However
  adding this entitlement to the R binary makes R crash on startup.

Otherwise `ps_environ` will return an empty set of environment variables
on macOS.

Issue 121 might have more information about this.

## See also

Other process handle functions:
[`ps_children()`](https://ps.r-lib.org/dev/reference/ps_children.md),
[`ps_cmdline()`](https://ps.r-lib.org/dev/reference/ps_cmdline.md),
[`ps_connections()`](https://ps.r-lib.org/dev/reference/ps_connections.md),
[`ps_cpu_times()`](https://ps.r-lib.org/dev/reference/ps_cpu_times.md),
[`ps_create_time()`](https://ps.r-lib.org/dev/reference/ps_create_time.md),
[`ps_cwd()`](https://ps.r-lib.org/dev/reference/ps_cwd.md),
[`ps_descent()`](https://ps.r-lib.org/dev/reference/ps_descent.md),
[`ps_exe()`](https://ps.r-lib.org/dev/reference/ps_exe.md),
[`ps_handle()`](https://ps.r-lib.org/dev/reference/ps_handle.md),
[`ps_interrupt()`](https://ps.r-lib.org/dev/reference/ps_interrupt.md),
[`ps_is_running()`](https://ps.r-lib.org/dev/reference/ps_is_running.md),
[`ps_kill()`](https://ps.r-lib.org/dev/reference/ps_kill.md),
[`ps_memory_info()`](https://ps.r-lib.org/dev/reference/ps_memory_info.md),
[`ps_name()`](https://ps.r-lib.org/dev/reference/ps_name.md),
[`ps_num_fds()`](https://ps.r-lib.org/dev/reference/ps_num_fds.md),
[`ps_num_threads()`](https://ps.r-lib.org/dev/reference/ps_num_threads.md),
[`ps_open_files()`](https://ps.r-lib.org/dev/reference/ps_open_files.md),
[`ps_pid()`](https://ps.r-lib.org/dev/reference/ps_pid.md),
[`ps_ppid()`](https://ps.r-lib.org/dev/reference/ps_ppid.md),
[`ps_resume()`](https://ps.r-lib.org/dev/reference/ps_resume.md),
[`ps_send_signal()`](https://ps.r-lib.org/dev/reference/ps_send_signal.md),
[`ps_shared_libs()`](https://ps.r-lib.org/dev/reference/ps_shared_libs.md),
[`ps_status()`](https://ps.r-lib.org/dev/reference/ps_status.md),
[`ps_suspend()`](https://ps.r-lib.org/dev/reference/ps_suspend.md),
[`ps_terminal()`](https://ps.r-lib.org/dev/reference/ps_terminal.md),
[`ps_terminate()`](https://ps.r-lib.org/dev/reference/ps_terminate.md),
[`ps_uids()`](https://ps.r-lib.org/dev/reference/ps_uids.md),
[`ps_username()`](https://ps.r-lib.org/dev/reference/ps_username.md)

## Examples

``` r
p <- ps_handle()
p
#> <ps::ps_handle> PID=7292, NAME=R, AT=2026-03-31 18:22:50.52
env <- ps_environ(p)
env[["R_HOME"]]
#> [1] "/opt/R/4.5.3/lib/R"
```
