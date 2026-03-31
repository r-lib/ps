# List the dynamically loaded libraries of a process

Note: this function currently only works on Windows.

## Usage

``` r
ps_shared_libs(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

Data frame with one column currently: `path`, the absolute path to the
loaded module or shared library. On Windows the list includes the
executable file itself.

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
[`ps_status()`](https://ps.r-lib.org/reference/ps_status.md),
[`ps_suspend()`](https://ps.r-lib.org/reference/ps_suspend.md),
[`ps_terminal()`](https://ps.r-lib.org/reference/ps_terminal.md),
[`ps_terminate()`](https://ps.r-lib.org/reference/ps_terminate.md),
[`ps_uids()`](https://ps.r-lib.org/reference/ps_uids.md),
[`ps_username()`](https://ps.r-lib.org/reference/ps_username.md)

Other shared library tools:
[`ps_shared_lib_users()`](https://ps.r-lib.org/reference/ps_shared_lib_users.md)

## Examples

``` r
if (FALSE) { # ps::ps_is_supported() && !ps:::is_cran_check() && ps::ps_os_type()[["WINDOWS"]]
# The loaded DLLs of the current process
ps_shared_libs()
}
```
