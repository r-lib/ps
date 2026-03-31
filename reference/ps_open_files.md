# Open files of a process

Note that in some IDEs, e.g. RStudio or R.app on macOS, the IDE itself
opens files from other threads, in addition to the files opened from the
main R thread.

## Usage

``` r
ps_open_files(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

Data frame with columns: `fd` and `path`. `fd` is numeric file
descriptor on POSIX systems, `NA` on Windows. `path` is an absolute path
to the file.

## Details

For a zombie process it throws a `zombie_process` error.

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
ps_open_files(p)
#> # A data frame: 16 × 2
#>       fd path                                                        
#>    <int> <chr>                                                       
#>  1     0 pipe:[32634]                                                
#>  2     1 pipe:[32635]                                                
#>  3     2 pipe:[32636]                                                
#>  4     3 /home/runner/work/_temp/ec69e292-a376-40b2-8d2e-7fdf0dbafbc0
#>  5     4 anon_inode:[eventpoll]                                      
#>  6     5 anon_inode:[io_uring]                                       
#>  7     6 pipe:[32644]                                                
#>  8     7 pipe:[32644]                                                
#>  9     8 pipe:[32645]                                                
#> 10     9 pipe:[32645]                                                
#> 11    10 anon_inode:[eventfd]                                        
#> 12    11 pipe:[31526]                                                
#> 13    12 pipe:[31526]                                                
#> 14    17 /tmp/RtmpW3UIAm/Rf1ccb34a44fd6 (deleted)                    
#> 15   142 pipe:[16757]                                                
#> 16   145 pipe:[16758]                                                
f <- file(tmp <- tempfile(), "w")
ps_open_files(p)
#> # A data frame: 17 × 2
#>       fd path                                                        
#>    <int> <chr>                                                       
#>  1     0 pipe:[32634]                                                
#>  2     1 pipe:[32635]                                                
#>  3     2 pipe:[32636]                                                
#>  4     3 /home/runner/work/_temp/ec69e292-a376-40b2-8d2e-7fdf0dbafbc0
#>  5     4 anon_inode:[eventpoll]                                      
#>  6     5 anon_inode:[io_uring]                                       
#>  7     6 pipe:[32644]                                                
#>  8     7 pipe:[32644]                                                
#>  9     8 pipe:[32645]                                                
#> 10     9 pipe:[32645]                                                
#> 11    10 anon_inode:[eventfd]                                        
#> 12    11 pipe:[31526]                                                
#> 13    12 pipe:[31526]                                                
#> 14    17 /tmp/RtmpW3UIAm/Rf1ccb34a44fd6 (deleted)                    
#> 15    18 /tmp/RtmpW3UIAm/file1ccb1004111e                            
#> 16   142 pipe:[16757]                                                
#> 17   145 pipe:[16758]                                                
close(f)
unlink(tmp)
ps_open_files(p)
#> # A data frame: 16 × 2
#>       fd path                                                        
#>    <int> <chr>                                                       
#>  1     0 pipe:[32634]                                                
#>  2     1 pipe:[32635]                                                
#>  3     2 pipe:[32636]                                                
#>  4     3 /home/runner/work/_temp/ec69e292-a376-40b2-8d2e-7fdf0dbafbc0
#>  5     4 anon_inode:[eventpoll]                                      
#>  6     5 anon_inode:[io_uring]                                       
#>  7     6 pipe:[32644]                                                
#>  8     7 pipe:[32644]                                                
#>  9     8 pipe:[32645]                                                
#> 10     9 pipe:[32645]                                                
#> 11    10 anon_inode:[eventfd]                                        
#> 12    11 pipe:[31526]                                                
#> 13    12 pipe:[31526]                                                
#> 14    17 /tmp/RtmpW3UIAm/Rf1ccb34a44fd6 (deleted)                    
#> 15   142 pipe:[16757]                                                
#> 16   145 pipe:[16758]                                                
```
