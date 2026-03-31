# Memory usage information

Memory usage information

## Usage

``` r
ps_memory_info(p = ps_handle())

ps_memory_full_info(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

Named real vector.

## Details

`ps_memory_info()` returns information about memory usage.

It returns a named vector. Portable fields:

- `rss`: "Resident Set Size", this is the non-swapped physical memory a
  process has used (bytes). On UNIX it matches "top"‘s 'RES' column (see
  doc). On Windows this is an alias for `wset` field and it matches
  "Memory" column of `taskmgr.exe`.

- `vmem`: "Virtual Memory Size", this is the total amount of virtual
  memory used by the process (bytes). On UNIX it matches "top"‘s 'VIRT'
  column (see doc). On Windows this is an alias for the `pagefile` field
  and it matches the "Working set (memory)" column of `taskmgr.exe`.

Non-portable fields:

- `shared`: (Linux) memory that could be potentially shared with other
  processes (bytes). This matches "top"‘s 'SHR' column (see doc).

- `text`: (Linux): aka 'TRS' (text resident set) the amount of memory
  devoted to executable code (bytes). This matches "top"‘s 'CODE' column
  (see doc).

- `data`: (Linux): aka 'DRS' (data resident set) the amount of physical
  memory devoted to other than executable code (bytes). It matches
  "top"‘s 'DATA' column (see doc).

- `lib`: (Linux): the memory used by shared libraries (bytes).

- `dirty`: (Linux): the amount of memory in dirty pages (bytes).

- `pfaults`: (macOS): number of page faults.

- `pageins`: (macOS): number of actual pageins.

For the explanation of Windows fields see the
[PROCESS_MEMORY_COUNTERS_EX](https://learn.microsoft.com/en-us/windows/win32/api/psapi/ns-psapi-process_memory_counters_ex)
structure.

`ps_memory_full_info()` returns all fields as `ps_memory_info()`, plus
additional information, but typically takes slightly longer to run, and
might not have access to some processes that `ps_memory_info()` can
query:

- `maxrss` maximum resident set size over the process's lifetime. This
  only works for the calling process, otherwise it is `NA_real_`.

- `uss`: Unique Set Size, this is the memory which is unique to a
  process and which would be freed if the process was terminated right
  now.

- `pss` (Linux only): Proportional Set Size, is the amount of memory
  shared with other processes, accounted in a way that the amount is
  divided evenly between the processes that share it. I.e. if a process
  has 10 MBs all to itself and 10 MBs shared with another process its
  PSS will be 15 MBs.

- `swap` (Linux only): amount of memory that has been swapped out to
  disk.

They both throw a `zombie_process()` error for zombie processes.

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
ps_memory_info(p)
#>       rss       vms    shared      text       lib      data     dirty 
#> 275009536 969789440  35807232      4096         0 766251008         0 
ps_memory_full_info(p)
#>       rss       vms    shared      text       lib      data     dirty 
#> 275009536 969789440  35807232      4096         0 766251008         0 
#>    maxrss       uss       pss      swap 
#> 274644992 255111168 259567616         0 
```
