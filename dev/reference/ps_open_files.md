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
[`ps_kill()`](https://ps.r-lib.org/dev/reference/ps_kill.md),
[`ps_memory_info()`](https://ps.r-lib.org/dev/reference/ps_memory_info.md),
[`ps_name()`](https://ps.r-lib.org/dev/reference/ps_name.md),
[`ps_num_fds()`](https://ps.r-lib.org/dev/reference/ps_num_fds.md),
[`ps_num_threads()`](https://ps.r-lib.org/dev/reference/ps_num_threads.md),
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
ps_open_files(p)
#> # A data frame: 16 × 2
#>       fd path                                                        
#>    <int> <chr>                                                       
#>  1     0 pipe:[33417]                                                
#>  2     1 pipe:[33418]                                                
#>  3     2 pipe:[33419]                                                
#>  4     3 /home/runner/work/_temp/799de2ec-62e9-4c84-a598-91a3869746ce
#>  5     4 anon_inode:[eventpoll]                                      
#>  6     5 anon_inode:[io_uring]                                       
#>  7     6 pipe:[30560]                                                
#>  8     7 pipe:[30560]                                                
#>  9     8 pipe:[30561]                                                
#> 10     9 pipe:[30561]                                                
#> 11    10 anon_inode:[eventfd]                                        
#> 12    11 pipe:[30564]                                                
#> 13    12 pipe:[30564]                                                
#> 14    17 /tmp/Rtmp8bailY/Rf1cb222d6d4c9 (deleted)                    
#> 15   142 pipe:[16585]                                                
#> 16   145 pipe:[16586]                                                
f <- file(tmp <- tempfile(), "w")
ps_open_files(p)
#> # A data frame: 17 × 2
#>       fd path                                                        
#>    <int> <chr>                                                       
#>  1     0 pipe:[33417]                                                
#>  2     1 pipe:[33418]                                                
#>  3     2 pipe:[33419]                                                
#>  4     3 /home/runner/work/_temp/799de2ec-62e9-4c84-a598-91a3869746ce
#>  5     4 anon_inode:[eventpoll]                                      
#>  6     5 anon_inode:[io_uring]                                       
#>  7     6 pipe:[30560]                                                
#>  8     7 pipe:[30560]                                                
#>  9     8 pipe:[30561]                                                
#> 10     9 pipe:[30561]                                                
#> 11    10 anon_inode:[eventfd]                                        
#> 12    11 pipe:[30564]                                                
#> 13    12 pipe:[30564]                                                
#> 14    17 /tmp/Rtmp8bailY/Rf1cb222d6d4c9 (deleted)                    
#> 15    18 /tmp/Rtmp8bailY/file1cb2296afbaa                            
#> 16   142 pipe:[16585]                                                
#> 17   145 pipe:[16586]                                                
close(f)
unlink(tmp)
ps_open_files(p)
#> # A data frame: 16 × 2
#>       fd path                                                        
#>    <int> <chr>                                                       
#>  1     0 pipe:[33417]                                                
#>  2     1 pipe:[33418]                                                
#>  3     2 pipe:[33419]                                                
#>  4     3 /home/runner/work/_temp/799de2ec-62e9-4c84-a598-91a3869746ce
#>  5     4 anon_inode:[eventpoll]                                      
#>  6     5 anon_inode:[io_uring]                                       
#>  7     6 pipe:[30560]                                                
#>  8     7 pipe:[30560]                                                
#>  9     8 pipe:[30561]                                                
#> 10     9 pipe:[30561]                                                
#> 11    10 anon_inode:[eventfd]                                        
#> 12    11 pipe:[30564]                                                
#> 13    12 pipe:[30564]                                                
#> 14    17 /tmp/Rtmp8bailY/Rf1cb222d6d4c9 (deleted)                    
#> 15   142 pipe:[16585]                                                
#> 16   145 pipe:[16586]                                                
```
