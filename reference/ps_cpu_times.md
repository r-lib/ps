# CPU times of the process

All times are measured in seconds:

- `user`: Amount of time that this process has been scheduled in user
  mode.

- `system`: Amount of time that this process has been scheduled in
  kernel mode

- `children_user`: On Linux, amount of time that this process's
  waited-for children have been scheduled in user mode.

- `children_system`: On Linux, Amount of time that this process's
  waited-for children have been scheduled in kernel mode.

## Usage

``` r
ps_cpu_times(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

Named real vector or length four: `user`, `system`, `children_user`,
`children_system`. The last two are `NA` on non-Linux systems.

## Details

Throws a `zombie_process()` error for zombie processes.

## See also

Other process handle functions:
[`ps_children()`](https://ps.r-lib.org/reference/ps_children.md),
[`ps_cmdline()`](https://ps.r-lib.org/reference/ps_cmdline.md),
[`ps_connections()`](https://ps.r-lib.org/reference/ps_connections.md),
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
ps_cpu_times(p)
#>            user          system   children_user children_system 
#>            7.27            0.70            1.07            0.92 
proc.time()
#>    user  system elapsed 
#>   8.352   1.632  11.900 
```
