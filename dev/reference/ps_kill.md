# Kill one or more processes

Kill the process with SIGKILL preemptively checking whether PID has been
reused. On Windows it uses `TerminateProcess()`.

## Usage

``` r
ps_kill(p = ps_handle(), grace = 200)
```

## Arguments

- p:

  Process handle, or a list of process handles.

- grace:

  Grace period, in milliseconds, used on Unix. If it is not zero, then
  `ps_kill()` first sends a `SIGTERM` signal to all processes in `p`. If
  some proccesses do not terminate within `grace` milliseconds after the
  `SIGTERM` signal, `ps_kill()` kills them by sending `SIGKILL` signals.

## Value

Character vector, with one element for each process handle in `p`. If
the process was already dead before `ps_kill()` tried to kill it, the
corresponding return value is `"dead"`. If `ps_kill()` just killed it,
it is `"killed"`.

## Details

Note that since ps version 1.8, `ps_kill()` does not error if the `p`
process (or some processes if `p` is a list) are already terminated.

## See also

Other process handle functions:
[`ps_children()`](https://ps.r-lib.org/dev/reference/ps_children.md),
[`ps_cmdline()`](https://ps.r-lib.org/dev/reference/ps_cmdline.md),
[`ps_connections()`](https://ps.r-lib.org/dev/reference/ps_connections.md),
[`ps_cpu_times()`](https://ps.r-lib.org/dev/reference/ps_cpu_times.md),
[`ps_create_time()`](https://ps.r-lib.org/dev/reference/ps_create_time.md),
[`ps_cwd()`](https://ps.r-lib.org/dev/reference/ps_cwd.md),
[`ps_descent()`](https://ps.r-lib.org/dev/reference/ps_descent.md),
[`ps_environ()`](https://ps.r-lib.org/dev/reference/ps_environ.md),
[`ps_exe()`](https://ps.r-lib.org/dev/reference/ps_exe.md),
[`ps_handle()`](https://ps.r-lib.org/dev/reference/ps_handle.md),
[`ps_interrupt()`](https://ps.r-lib.org/dev/reference/ps_interrupt.md),
[`ps_is_running()`](https://ps.r-lib.org/dev/reference/ps_is_running.md),
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
px <- processx::process$new("sleep", "10")
p <- ps_handle(px$get_pid())
p
#> <ps::ps_handle> PID=8403, NAME=sleep, AT=2026-03-31 18:23:06.08
ps_kill(p)
#> [1] "terminated"
p
#> <ps::ps_handle> PID=8403, NAME=???, AT=2026-03-31 18:23:06.08
ps_is_running(p)
#> [1] FALSE
px$get_exit_status()
#> [1] -15
```
