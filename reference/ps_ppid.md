# Parent pid or parent process of a process

`ps_ppid()` returns the parent pid, `ps_parent()` returns a `ps_handle`
of the parent.

## Usage

``` r
ps_ppid(p = ps_handle())

ps_parent(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

`ps_ppid()` returns and integer scalar, the pid of the parent of `p`.
`ps_parent()` returns a `ps_handle`.

## Details

On POSIX systems, if the parent process terminates, another process
(typically the pid 1 process) is marked as parent. `ps_ppid()` and
`ps_parent()` will return this process then.

Both `ps_ppid()` and `ps_parent()` work for zombie processes.

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
[`ps_resume()`](https://ps.r-lib.org/reference/ps_resume.md),
[`ps_send_signal()`](https://ps.r-lib.org/reference/ps_send_signal.md),
[`ps_shared_libs()`](https://ps.r-lib.org/reference/ps_shared_libs.md),
[`ps_status()`](https://ps.r-lib.org/reference/ps_status.md),
[`ps_suspend()`](https://ps.r-lib.org/reference/ps_suspend.md),
[`ps_terminal()`](https://ps.r-lib.org/reference/ps_terminal.md),
[`ps_terminate()`](https://ps.r-lib.org/reference/ps_terminate.md),
[`ps_uids()`](https://ps.r-lib.org/reference/ps_uids.md),
[`ps_username()`](https://ps.r-lib.org/reference/ps_username.md)

## Examples

``` r
p <- ps_handle()
p
#> <ps::ps_handle> PID=7936, NAME=R, AT=2026-03-31 18:14:52.66
ps_ppid(p)
#> [1] 2167
ps_parent(p)
#> <ps::ps_handle> PID=2167, NAME=Runner.Worker, AT=2026-03-31 18:13:32.49
```
