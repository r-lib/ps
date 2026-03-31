# List network connections of a process

For a zombie process it throws a `zombie_process` error.

## Usage

``` r
ps_connections(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

Data frame, with columns:

- `fd`: integer file descriptor on POSIX systems, `NA` on Windows.

- `family`: Address family, string, typically `AF_UNIX`, `AF_INET` or
  `AF_INET6`.

- `type`: Socket type, string, typically `SOCK_STREAM` (TCP) or
  `SOCK_DGRAM` (UDP).

- `laddr`: Local address, string, `NA` for UNIX sockets.

- `lport`: Local port, integer, `NA` for UNIX sockets.

- `raddr`: Remote address, string, `NA` for UNIX sockets. This is always
  `NA` for `AF_INET` sockets on Linux.

- `rport`: Remote port, integer, `NA` for UNIX sockets.

- `state`: Socket state, e.g. `CONN_ESTABLISHED`, etc. It is `NA` for
  UNIX sockets.

## See also

Other process handle functions:
[`ps_children()`](https://ps.r-lib.org/dev/reference/ps_children.md),
[`ps_cmdline()`](https://ps.r-lib.org/dev/reference/ps_cmdline.md),
[`ps_cpu_times()`](https://ps.r-lib.org/dev/reference/ps_cpu_times.md),
[`ps_create_time()`](https://ps.r-lib.org/dev/reference/ps_create_time.md),
[`ps_cwd()`](https://ps.r-lib.org/dev/reference/ps_cwd.md),
[`ps_descent()`](https://ps.r-lib.org/dev/reference/ps_descent.md),
[`ps_environ()`](https://ps.r-lib.org/dev/reference/ps_environ.md),
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
ps_connections(p)
#> # A data frame: 4 × 8
#>      fd family  type        laddr      lport raddr          rport state
#>   <int> <chr>   <chr>       <chr>      <int> <chr>          <int> <chr>
#> 1    14 AF_INET SOCK_STREAM 10.1.0.227 37666 142.251.214.42   443 CONN…
#> 2    13 AF_INET SOCK_STREAM 10.1.0.227 53234 108.138.246.1…   443 CONN…
#> 3    16 AF_INET SOCK_STREAM 10.1.0.227 60590 108.138.246.1…    80 CONN…
#> 4    15 AF_INET SOCK_STREAM 10.1.0.227 35528 142.251.218.1…   443 CONN…
sc <- socketConnection("httpbin.org", port = 80)
ps_connections(p)
#> # A data frame: 5 × 8
#>      fd family  type        laddr      lport raddr          rport state
#>   <int> <chr>   <chr>       <chr>      <int> <chr>          <int> <chr>
#> 1    18 AF_INET SOCK_STREAM 10.1.0.227 34382 52.6.211.202      80 CONN…
#> 2    14 AF_INET SOCK_STREAM 10.1.0.227 37666 142.251.214.42   443 CONN…
#> 3    13 AF_INET SOCK_STREAM 10.1.0.227 53234 108.138.246.1…   443 CONN…
#> 4    16 AF_INET SOCK_STREAM 10.1.0.227 60590 108.138.246.1…    80 CONN…
#> 5    15 AF_INET SOCK_STREAM 10.1.0.227 35528 142.251.218.1…   443 CONN…
close(sc)
ps_connections(p)
#> # A data frame: 4 × 8
#>      fd family  type        laddr      lport raddr          rport state
#>   <int> <chr>   <chr>       <chr>      <int> <chr>          <int> <chr>
#> 1    14 AF_INET SOCK_STREAM 10.1.0.227 37666 142.251.214.42   443 CONN…
#> 2    13 AF_INET SOCK_STREAM 10.1.0.227 53234 108.138.246.1…   443 CONN…
#> 3    16 AF_INET SOCK_STREAM 10.1.0.227 60590 108.138.246.1…    80 CONN…
#> 4    15 AF_INET SOCK_STREAM 10.1.0.227 35528 142.251.218.1…   443 CONN…
```
