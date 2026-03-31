# Changelog

## ps 1.9.2

CRAN release: 2026-03-31

- New [`ps_string()`](https://ps.r-lib.org/reference/ps_string.md) for
  uniquely identifying a process
  ([\#208](https://github.com/r-lib/ps/issues/208),
  [@dansmith01](https://github.com/dansmith01)).

## ps 1.9.1

CRAN release: 2025-04-12

- ps now builds correctly on Alpine Linux (3.19) on R 4.5.0.

## ps 1.9.0

CRAN release: 2025-02-18

- [`ps_memory_full_info()`](https://ps.r-lib.org/reference/ps_memory_info.md)
  now contains `maxrss`, the maximum resident set size for the calling
  process.

- New `columns` argument in
  [`ps()`](https://ps.r-lib.org/reference/ps.md), to customize what data
  is returned ([\#138](https://github.com/r-lib/ps/issues/138)).

## ps 1.8.1

CRAN release: 2024-10-28

- ps can now be installed again on unsupported platforms.

## ps 1.8.0

CRAN release: 2024-09-12

- New [`ps_apps()`](https://ps.r-lib.org/reference/ps_apps.md) function
  to list all running applications on macOS.

- New function
  [`ps_disk_io_counters()`](https://ps.r-lib.org/reference/ps_disk_io_counters.md)
  to query disk I/O counters
  ([\#145](https://github.com/r-lib/ps/issues/145),
  [@michaelwalshe](https://github.com/michaelwalshe)).

- New [`ps_fs_info()`](https://ps.r-lib.org/reference/ps_fs_info.md) to
  query information about the file system of one or more files or
  directories.

- New [`ps_wait()`](https://ps.r-lib.org/reference/ps_wait.md) to start
  an interruptible wait on multiple processes, with a timeout
  ([\#166](https://github.com/r-lib/ps/issues/166)).

- [`ps_handle()`](https://ps.r-lib.org/reference/ps_handle.md) now
  allows a numeric (double) scalar as the pid, as long as its value is
  integer.

- [`ps_send_signal()`](https://ps.r-lib.org/reference/ps_send_signal.md),
  [`ps_suspend()`](https://ps.r-lib.org/reference/ps_suspend.md),
  [`ps_resume()`](https://ps.r-lib.org/reference/ps_resume.md),
  [`ps_terminate()`](https://ps.r-lib.org/reference/ps_terminate.md),
  [`ps_kill()`](https://ps.r-lib.org/reference/ps_kill.md), and
  [`ps_interrupt()`](https://ps.r-lib.org/reference/ps_interrupt.md) can
  now operate on multiple processes, if passed a list of process
  handles.

- [`ps_kill()`](https://ps.r-lib.org/reference/ps_kill.md) and
  [`ps_kill_tree()`](https://ps.r-lib.org/reference/ps_kill_tree.md)
  have a new `grace` argument. On Unix, if this argument is not zero,
  then [`ps_kill()`](https://ps.r-lib.org/reference/ps_kill.md) first
  sends a `TERM` signal, and waits for the processes to quit gracefully,
  via [`ps_wait()`](https://ps.r-lib.org/reference/ps_wait.md). The
  processes that are still alive after the grace period are then killed
  with `SIGKILL`.

- [`ps_status()`](https://ps.r-lib.org/reference/ps_status.md) (and thus
  [`ps()`](https://ps.r-lib.org/reference/ps.md)) is now better at
  getting the correct status of processes on macOS. This usually
  requires calling the external `ps` tool. See `?ps_status()` on how to
  opt out from the new behavior
  ([\#31](https://github.com/r-lib/ps/issues/31)).

## ps 1.7.7

CRAN release: 2024-07-02

- [`ps_cpu_times()`](https://ps.r-lib.org/reference/ps_cpu_times.md)
  values are now correct on newer arm64 macOS.

## ps 1.7.6

CRAN release: 2024-01-18

- [`ps_name()`](https://ps.r-lib.org/reference/ps_name.md) now does not
  fail in the rare case when
  [`ps_cmdline()`](https://ps.r-lib.org/reference/ps_cmdline.md) returns
  an empty vector ([\#150](https://github.com/r-lib/ps/issues/150)).

- [`ps_system_cpu_times()`](https://ps.r-lib.org/reference/ps_system_cpu_times.md)
  now returns CPU times divided by the HZ as reported by CLK_TCK,
  in-line with other OS’s and the per-process version.
  ([\#144](https://github.com/r-lib/ps/issues/144),
  [@michaelwalshe](https://github.com/michaelwalshe)).

## ps 1.7.5

CRAN release: 2023-04-18

No user visible changes.

## ps 1.7.4

CRAN release: 2023-04-02

- [`ps::ps_get_cpu_affinity()`](https://ps.r-lib.org/reference/ps_get_cpu_affinity.md)
  now works for other processes on Linux, not only the calling process.

## ps 1.7.3

CRAN release: 2023-03-21

- The output of
  [`ps_disk_usage()`](https://ps.r-lib.org/reference/ps_disk_usage.md),
  [`ps_disk_partitions()`](https://ps.r-lib.org/reference/ps_disk_partitions.md)
  and
  [`ps_shared_lib_users()`](https://ps.r-lib.org/reference/ps_shared_lib_users.md)
  now do not include a spurious `stringsAsFactors` column.

## ps 1.7.2

CRAN release: 2022-10-26

- `ps_system_memory()$percent` now returns a number scaled between 0 and
  100 on Windows, rather than between 0 and 1
  ([\#131](https://github.com/r-lib/ps/issues/131),
  [@francisbarton](https://github.com/francisbarton)).

## ps 1.7.1

CRAN release: 2022-06-18

- ps now returns data frames instead of tibbles. While data frames and
  tibbles are very similar, they are not completely compatible. To
  convert the output of ps to tibbles call the
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  function on them.

- [`ps()`](https://ps.r-lib.org/reference/ps.md) now does not fail if
  both `user` and `after` are specified
  ([\#129](https://github.com/r-lib/ps/issues/129)).

## ps 1.7.0

CRAN release: 2022-04-23

- ps now compiles on platforms that enable OpenMP
  ([\#109](https://github.com/r-lib/ps/issues/109)).

- New functions
  [`ps_get_cpu_affinity()`](https://ps.r-lib.org/reference/ps_get_cpu_affinity.md)
  and
  [`ps_set_cpu_affinity()`](https://ps.r-lib.org/reference/ps_get_cpu_affinity.md)
  to query and set CPU affinity
  ([\#123](https://github.com/r-lib/ps/issues/123)).

- [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md)
  now does not mix up `rss` and `vms` on Linux.

- [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md)
  now reports memory in bytes instead of pages on Linux
  ([\#115](https://github.com/r-lib/ps/issues/115))

## ps 1.6.0

CRAN release: 2021-02-28

- New function
  [`ps_system_cpu_times()`](https://ps.r-lib.org/reference/ps_system_cpu_times.md)
  to calculate system CPU times.

- New function
  [`ps_loadavg()`](https://ps.r-lib.org/reference/ps_loadavg.md) to show
  the Unix style load average.

## ps 1.5.0

CRAN release: 2020-12-05

- New function
  [`ps_shared_libs()`](https://ps.r-lib.org/reference/ps_shared_libs.md)
  to list the loaded shared libraries of a process, on Windows.

- New function
  [`ps_shared_lib_users()`](https://ps.r-lib.org/reference/ps_shared_lib_users.md)
  to list all processes that loaded a certain shared library, on
  Windows.

- New function
  [`ps_descent()`](https://ps.r-lib.org/reference/ps_descent.md) to
  query the ancestry of a process.

## ps 1.4.0

CRAN release: 2020-10-07

- ps is now under the MIT license.

- Process functions now default to the calling R process. So e.g. you
  can write simply
  [`ps_connections()`](https://ps.r-lib.org/reference/ps_connections.md)
  to list all network connections of the current process, instead of
  `ps_connections(ps_handle())`.

- New [`ps_get_nice()`](https://ps.r-lib.org/reference/ps_get_nice.md)
  and [`ps_set_nice()`](https://ps.r-lib.org/reference/ps_get_nice.md)
  functions to get and set the priority of a process
  ([\#89](https://github.com/r-lib/ps/issues/89)).

- New
  [`ps_system_memory()`](https://ps.r-lib.org/reference/ps_system_memory.md)
  and
  [`ps_system_swap()`](https://ps.r-lib.org/reference/ps_system_swap.md)
  functions, to return information about system memory and swap usage.

- New
  [`ps_disk_partitions()`](https://ps.r-lib.org/reference/ps_disk_partitions.md)
  and
  [`ps_disk_usage()`](https://ps.r-lib.org/reference/ps_disk_usage.md)
  functions, they return information about file systems, similarly to
  the `mount` and `df` Unix commands.

- New [`ps_tty_size()`](https://ps.r-lib.org/reference/ps_tty_size.md)
  function to query the size of the terminal.

- Fixed an issue in
  [`CleanupReporter()`](https://ps.r-lib.org/reference/CleanupReporter.md)
  that triggered random failures on macOS.

## ps 1.3.4

CRAN release: 2020-08-11

- [`ps_cpu_count()`](https://ps.r-lib.org/reference/ps_cpu_count.md) now
  reports the correct number on Windows, even if the package binary was
  built on a Windows version with a different API
  ([\#77](https://github.com/r-lib/ps/issues/77)).

## ps 1.3.3

CRAN release: 2020-05-08

- New function [`errno()`](https://ps.r-lib.org/reference/errno.md)
  returns a table of `errno.h` error codes and their description.

- ps now compiles again on Solaris.

## ps 1.3.2

CRAN release: 2020-02-13

- ps now compiles again on unsupported platforms like Solaris.

## ps 1.3.1

CRAN release: 2020-02-12

- Fixed an installation problem on some Windows versions, where the
  output of `cmd /c ver` looks different
  ([\#69](https://github.com/r-lib/ps/issues/69)).

## ps 1.3.0

CRAN release: 2018-12-21

- New [`ps_cpu_count()`](https://ps.r-lib.org/reference/ps_cpu_count.md)
  function returns the number of logical or physical processors.

## ps 1.2.1

CRAN release: 2018-11-06

- Fix a crash on Linux, that happened at load time
  ([\#50](https://github.com/r-lib/ps/issues/50)).

## ps 1.2.0

CRAN release: 2018-10-16

- New
  [`ps_connections()`](https://ps.r-lib.org/reference/ps_connections.md)
  to list network connections. The
  [`CleanupReporter()`](https://ps.r-lib.org/reference/CleanupReporter.md)
  testthat reporter can check for leftover open network connections in
  test cases.

- [`ps_open_files()`](https://ps.r-lib.org/reference/ps_open_files.md)
  does not include open sockets now on Linux, they are rather included
  in
  [`ps_connections()`](https://ps.r-lib.org/reference/ps_connections.md).

- [`CleanupReporter()`](https://ps.r-lib.org/reference/CleanupReporter.md)
  now ignores `/dev/urandom`, some packages (curl, openssl, etc.) keep
  this file open.

- Fix [`ps()`](https://ps.r-lib.org/reference/ps.md) printing without
  the tibble package ([\#43](https://github.com/r-lib/ps/issues/43)).

- Fix compilation with ICC
  ([\#39](https://github.com/r-lib/ps/issues/39)).

- Fix a crash on Linux ([\#47](https://github.com/r-lib/ps/issues/47)).

## ps 1.1.0

CRAN release: 2018-08-10

- New [`ps_num_fds()`](https://ps.r-lib.org/reference/ps_num_fds.md)
  returns the number of open files/handles.

- New
  [`ps_open_files()`](https://ps.r-lib.org/reference/ps_open_files.md)
  lists all open files of a process.

- New [`ps_interrupt()`](https://ps.r-lib.org/reference/ps_interrupt.md)
  interrupts a process. It sends a `SIGINT` signal on POSIX systems, and
  it can send CTRL+C or CTRL+BREAK events on Windows.

- New [`ps_users()`](https://ps.r-lib.org/reference/ps_users.md) lists
  users connected to the system.

- New
  [`ps_mark_tree()`](https://ps.r-lib.org/reference/ps_kill_tree.md),
  [`ps_find_tree()`](https://ps.r-lib.org/reference/ps_kill_tree.md),
  [`ps_kill_tree()`](https://ps.r-lib.org/reference/ps_kill_tree.md),
  [`with_process_cleanup()`](https://ps.r-lib.org/reference/ps_kill_tree.md):
  functions to mark and clean up child processes.

- New `CleanupReporter`, to be used with testthat: it checks for
  leftover child processes and open files in `test_that()` blocks.

## ps 1.0.0

CRAN release: 2018-07-22

First released version.
