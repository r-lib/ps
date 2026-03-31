# Current process status

One of the following:

- `"idle"`: Process being created by fork, or process has been sleeping
  for a long time. macOS only.

- `"running"`: Currently runnable on macOS and Windows. Actually running
  on Linux.

- `"sleeping"` Sleeping on a wait or poll.

- `"disk_sleep"` Uninterruptible sleep, waiting for an I/O operation
  (Linux only).

- `"stopped"` Stopped, either by a job control signal or because it is
  being traced.

- `"uninterruptible"` Process is in uninterruptible wait. macOS only.

- `"tracing_stop"` Stopped for tracing (Linux only).

- `"zombie"` Zombie. Finished, but parent has not read out the exit
  status yet.

- `"dead"` Should never be seen (Linux).

- `"wake_kill"` Received fatal signal (Linux only).

- `"waking"` Paging (Linux only, not valid since the 2.6.xx kernel).

## Usage

``` r
ps_status(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

Character scalar.

## Details

It might return `NA_character_` on macOS.

Works for zombie processes.

## Note on macOS

On macOS `ps_status()` often falls back to calling the external `ps`
program, because macOS does not let R access the status of most other
processes. Notably, it is usually able to access the status of other R
processes.

The external `ps` program always runs as the root user, and it also has
special entitlements, so it can typically access the status of most
processes.

If this behavior is problematic for you, e.g. because calling an
external program is too slow, set the `ps.no_external_ps` option to
`TRUE`:

    options(ps.no_external_ps = TRUE)

Note that setting this option to `TRUE` will cause `ps_status()` to
return `NA_character_` for most processes.

## See also

Other process handle functions:
[`ps_children()`](https://ps.r-lib.org/reference/ps_children.md),
[`ps_cmdline()`](https://ps.r-lib.org/reference/ps_cmdline.md),
[`ps_connections()`](https://ps.r-lib.org/reference/ps_connections.md),
[`ps_cpu_times()`](https://ps.r-lib.org/reference/ps_cpu_times.md),
[`ps_create_time()`](https://ps.r-lib.org/reference/ps_create_time.md),
[`ps_cwd()`](https://ps.r-lib.org/reference/ps_cwd.md),
[`ps_descent()`](https://ps.r-lib.org/reference/ps_descent.md),
[`ps_environ()`](https://ps.r-lib.org/reference/ps_environ.md),
[`ps_exe()`](https://ps.r-lib.org/reference/ps_exe.md),
[`ps_handle()`](https://ps.r-lib.org/reference/ps_handle.md),
[`ps_interrupt()`](https://ps.r-lib.org/reference/ps_interrupt.md),
[`ps_is_running()`](https://ps.r-lib.org/reference/ps_is_running.md),
[`ps_kill()`](https://ps.r-lib.org/reference/ps_kill.md),
[`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md),
[`ps_name()`](https://ps.r-lib.org/reference/ps_name.md),
[`ps_num_fds()`](https://ps.r-lib.org/reference/ps_num_fds.md),
[`ps_num_threads()`](https://ps.r-lib.org/reference/ps_num_threads.md),
[`ps_open_files()`](https://ps.r-lib.org/reference/ps_open_files.md),
[`ps_pid()`](https://ps.r-lib.org/reference/ps_pid.md),
[`ps_ppid()`](https://ps.r-lib.org/reference/ps_ppid.md),
[`ps_resume()`](https://ps.r-lib.org/reference/ps_resume.md),
[`ps_send_signal()`](https://ps.r-lib.org/reference/ps_send_signal.md),
[`ps_shared_libs()`](https://ps.r-lib.org/reference/ps_shared_libs.md),
[`ps_suspend()`](https://ps.r-lib.org/reference/ps_suspend.md),
[`ps_terminal()`](https://ps.r-lib.org/reference/ps_terminal.md),
[`ps_terminate()`](https://ps.r-lib.org/reference/ps_terminate.md),
[`ps_uids()`](https://ps.r-lib.org/reference/ps_uids.md),
[`ps_username()`](https://ps.r-lib.org/reference/ps_username.md)

## Examples

``` r
p <- ps_handle()
p
#> <ps::ps_handle> PID=7371, NAME=R, AT=2026-03-31 18:20:27.39
ps_status(p)
#> [1] "running"
```
