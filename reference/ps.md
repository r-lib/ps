# Process table

Data frame with the currently running processes.

## Usage

``` r
ps(user = NULL, after = NULL, columns = NULL)
```

## Arguments

- user:

  Username, to filter the results to matching processes.

- after:

  Start time (`POSIXt`), to filter the results to processes that started
  after this.

- columns:

  Columns to include in the result. If `NULL` (the default), then a
  default set of columns are returned, see below. The columns are shown
  in the same order they are specified in `columns`, but each column is
  included at most once. Use `"*"` to include all possible columns, and
  prefix a column name with `-` to remove it.

## Value

Data frame, see columns below.

## Details

Columns shown by default, if `columns` is not given or `NULL`:

- `pid`: Process ID.

- `ppid`: Process ID of parent process.

- `name`: Process name.

- `username`: Name of the user (real uid on POSIX).

- `status`: I.e. *running*, *sleeping*, etc.

- `user`: User CPU time.

- `system`: System CPU time.

- `rss`: Resident set size, the amount of memory the process currently
  uses. Does not include memory that is swapped out. It does include
  shared libraries.

- `vms`: Virtual memory size. All memory the process has access to.

- `created`: Time stamp when the process was created.

- `ps_handle`: `ps_handle` objects, in a list column.

Additional columns that can be requested via `columns`:

- `cmdline`: Command line, in a single string, from
  [`ps_cmdline()`](https://ps.r-lib.org/reference/ps_cmdline.md).

- `vcmdline`: Like `cmdline`, but each command line argument in a
  separate string.

- `cwd`: Current working directory, from
  [`ps_cwd()`](https://ps.r-lib.org/reference/ps_cwd.md).

- `exe`: Path of the executable of the process, from
  [`ps_exe()`](https://ps.r-lib.org/reference/ps_exe.md).

- `num_fds`: Number of open file descriptors, from
  [`ps_num_fds()`](https://ps.r-lib.org/reference/ps_num_fds.md).

- `num_threads`: Number of threads, from
  [`ps_num_threads()`](https://ps.r-lib.org/reference/ps_num_threads.md).

- `cpu_children_user`: See
  [`ps_cpu_times()`](https://ps.r-lib.org/reference/ps_cpu_times.md).

- `cpu_children_system`: See
  [`ps_cpu_times()`](https://ps.r-lib.org/reference/ps_cpu_times.md).

- `terminal`: Terminal device, from
  [`ps_terminal()`](https://ps.r-lib.org/reference/ps_terminal.md).

- `uid_real`: Real user id, from
  [`ps_uids()`](https://ps.r-lib.org/reference/ps_uids.md).

- `uid_effective`: Effective user id, from
  [`ps_uids()`](https://ps.r-lib.org/reference/ps_uids.md).

- `uid_saved`: Saved user id, from
  [`ps_uids()`](https://ps.r-lib.org/reference/ps_uids.md).

- `gid_real`: Real group id, from
  [`ps_gids()`](https://ps.r-lib.org/reference/ps_uids.md).

- `gid_effective`: Effective group id, from
  [`ps_gids()`](https://ps.r-lib.org/reference/ps_uids.md).

- `gid_saved`: Saved group id, from
  [`ps_gids()`](https://ps.r-lib.org/reference/ps_uids.md).

- `mem_shared`: See
  [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_text`: See
  [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_data`: See
  [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_lib`: See
  [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_dirty`: See
  [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_pfaults`: See
  [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_pageins`: See
  [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_maxrss`: See
  [`ps_memory_full_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_uss`: See
  [`ps_memory_full_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_pss`: See
  [`ps_memory_full_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

- `mem_swap`: See
  [`ps_memory_full_info()`](https://ps.r-lib.org/reference/ps_memory_info.md).

Use `"*"` in `columns` to include all columns.
