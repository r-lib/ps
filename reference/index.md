# Package index

## List processes

- [`ps()`](https://ps.r-lib.org/reference/ps.md) : Process table
- [`ps_apps()`](https://ps.r-lib.org/reference/ps_apps.md) : List
  currently running applications
- [`ps_pids()`](https://ps.r-lib.org/reference/ps_pids.md) : Ids of all
  processes on the system

## Process query API

- [`ps_children()`](https://ps.r-lib.org/reference/ps_children.md) :
  List of child processes (process objects) of the process. Note that
  this typically requires enumerating all processes on the system, so it
  is a costly operation.
- [`ps_cmdline()`](https://ps.r-lib.org/reference/ps_cmdline.md) :
  Command line of the process
- [`ps_cpu_times()`](https://ps.r-lib.org/reference/ps_cpu_times.md) :
  CPU times of the process
- [`ps_create_time()`](https://ps.r-lib.org/reference/ps_create_time.md)
  : Start time of a process
- [`ps_cwd()`](https://ps.r-lib.org/reference/ps_cwd.md) : Process
  current working directory as an absolute path.
- [`ps_descent()`](https://ps.r-lib.org/reference/ps_descent.md) : Query
  the ancestry of a process
- [`ps_environ()`](https://ps.r-lib.org/reference/ps_environ.md)
  [`ps_environ_raw()`](https://ps.r-lib.org/reference/ps_environ.md) :
  Environment variables of a process
- [`ps_exe()`](https://ps.r-lib.org/reference/ps_exe.md) : Full path of
  the executable of a process
- [`ps_handle()`](https://ps.r-lib.org/reference/ps_handle.md)
  [`as.character(`*`<ps_handle>`*`)`](https://ps.r-lib.org/reference/ps_handle.md)
  [`format(`*`<ps_handle>`*`)`](https://ps.r-lib.org/reference/ps_handle.md)
  [`print(`*`<ps_handle>`*`)`](https://ps.r-lib.org/reference/ps_handle.md)
  : Create a process handle
- [`ps_is_running()`](https://ps.r-lib.org/reference/ps_is_running.md) :
  Checks whether a process is running
- [`ps_memory_info()`](https://ps.r-lib.org/reference/ps_memory_info.md)
  [`ps_memory_full_info()`](https://ps.r-lib.org/reference/ps_memory_info.md)
  : Memory usage information
- [`ps_name()`](https://ps.r-lib.org/reference/ps_name.md) : Process
  name
- [`ps_num_threads()`](https://ps.r-lib.org/reference/ps_num_threads.md)
  : Number of threads
- [`ps_pid()`](https://ps.r-lib.org/reference/ps_pid.md) : Pid of a
  process handle
- [`ps_ppid()`](https://ps.r-lib.org/reference/ps_ppid.md)
  [`ps_parent()`](https://ps.r-lib.org/reference/ps_ppid.md) : Parent
  pid or parent process of a process
- [`ps_shared_libs()`](https://ps.r-lib.org/reference/ps_shared_libs.md)
  : List the dynamically loaded libraries of a process
- [`ps_status()`](https://ps.r-lib.org/reference/ps_status.md) : Current
  process status
- [`ps_terminal()`](https://ps.r-lib.org/reference/ps_terminal.md) :
  Terminal device of the process
- [`ps_uids()`](https://ps.r-lib.org/reference/ps_uids.md)
  [`ps_gids()`](https://ps.r-lib.org/reference/ps_uids.md) : User ids
  and group ids of the process
- [`ps_username()`](https://ps.r-lib.org/reference/ps_username.md) :
  Owner of the process

## Files and Network Connections

- [`ps_connections()`](https://ps.r-lib.org/reference/ps_connections.md)
  : List network connections of a process
- [`ps_num_fds()`](https://ps.r-lib.org/reference/ps_num_fds.md) :
  Number of open file descriptors
- [`ps_open_files()`](https://ps.r-lib.org/reference/ps_open_files.md) :
  Open files of a process

## Process manipulation

- [`ps_interrupt()`](https://ps.r-lib.org/reference/ps_interrupt.md) :
  Interrupt a process
- [`ps_kill()`](https://ps.r-lib.org/reference/ps_kill.md) : Kill one or
  more processes
- [`ps_resume()`](https://ps.r-lib.org/reference/ps_resume.md) : Resume
  (continue) a stopped process
- [`ps_send_signal()`](https://ps.r-lib.org/reference/ps_send_signal.md)
  : Send signal to a process
- [`ps_suspend()`](https://ps.r-lib.org/reference/ps_suspend.md) :
  Suspend (stop) the process
- [`ps_terminate()`](https://ps.r-lib.org/reference/ps_terminate.md) :
  Terminate a Unix process
- [`ps_get_cpu_affinity()`](https://ps.r-lib.org/reference/ps_get_cpu_affinity.md)
  [`ps_set_cpu_affinity()`](https://ps.r-lib.org/reference/ps_get_cpu_affinity.md)
  : Query or set CPU affinity
- [`ps_windows_nice_values()`](https://ps.r-lib.org/reference/ps_get_nice.md)
  [`ps_get_nice()`](https://ps.r-lib.org/reference/ps_get_nice.md)
  [`ps_set_nice()`](https://ps.r-lib.org/reference/ps_get_nice.md) : Get
  or set the priority of a process
- [`ps_wait()`](https://ps.r-lib.org/reference/ps_wait.md) : Wait for
  one or more processes to terminate, with a timeout

## Users

- [`ps_users()`](https://ps.r-lib.org/reference/ps_users.md) : List
  users connected to the system

## Disks and files

- [`ps_disk_partitions()`](https://ps.r-lib.org/reference/ps_disk_partitions.md)
  : List all mounted partitions
- [`ps_disk_usage()`](https://ps.r-lib.org/reference/ps_disk_usage.md) :
  Disk usage statistics, per partition
- [`ps_disk_io_counters()`](https://ps.r-lib.org/reference/ps_disk_io_counters.md)
  : System-wide disk I/O counters
- [`ps_fs_info()`](https://ps.r-lib.org/reference/ps_fs_info.md) : File
  system information for files
- [`ps_fs_mount_point()`](https://ps.r-lib.org/reference/ps_fs_mount_point.md)
  : Find the mount point of a file or directory
- [`ps_fs_stat()`](https://ps.r-lib.org/reference/ps_fs_stat.md) : File
  status

## Other system information

- [`ps_boot_time()`](https://ps.r-lib.org/reference/ps_boot_time.md) :
  Boot time of the system
- [`ps_os_type()`](https://ps.r-lib.org/reference/ps_os_type.md)
  [`ps_is_supported()`](https://ps.r-lib.org/reference/ps_os_type.md) :
  Query the type of the OS
- [`ps_cpu_count()`](https://ps.r-lib.org/reference/ps_cpu_count.md) :
  Number of logical or physical CPUs
- [`ps_tty_size()`](https://ps.r-lib.org/reference/ps_tty_size.md) :
  Query the size of the current terminal
- [`ps_loadavg()`](https://ps.r-lib.org/reference/ps_loadavg.md) :
  Return the average system load over the last 1, 5 and 15 minutes as a
  tuple.
- [`ps_shared_lib_users()`](https://ps.r-lib.org/reference/ps_shared_lib_users.md)
  : List all processes that loaded a shared library
- [`ps_system_cpu_times()`](https://ps.r-lib.org/reference/ps_system_cpu_times.md)
  : System CPU times.
- [`ps_system_memory()`](https://ps.r-lib.org/reference/ps_system_memory.md)
  : Statistics about system memory usage
- [`ps_system_swap()`](https://ps.r-lib.org/reference/ps_system_swap.md)
  : System swap memory statistics

## Process tree cleanup

- [`ps_mark_tree()`](https://ps.r-lib.org/reference/ps_kill_tree.md)
  [`with_process_cleanup()`](https://ps.r-lib.org/reference/ps_kill_tree.md)
  [`ps_find_tree()`](https://ps.r-lib.org/reference/ps_kill_tree.md)
  [`ps_kill_tree()`](https://ps.r-lib.org/reference/ps_kill_tree.md) :
  Mark a process and its (future) child tree
- [`CleanupReporter()`](https://ps.r-lib.org/reference/CleanupReporter.md)
  : testthat reporter that checks if child processes are cleaned up in
  tests

## Utility functions

- [`ps_os_type()`](https://ps.r-lib.org/reference/ps_os_type.md)
  [`ps_is_supported()`](https://ps.r-lib.org/reference/ps_os_type.md) :
  Query the type of the OS

- [`ps_string()`](https://ps.r-lib.org/reference/ps_string.md) :

  Encode a `ps_handle` as a short string

- [`signals()`](https://ps.r-lib.org/reference/signals.md) : List of all
  supported signals

- [`errno()`](https://ps.r-lib.org/reference/errno.md) : List of 'errno'
  error codes
