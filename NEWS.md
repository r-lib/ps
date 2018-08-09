
# ps 1.1.0

* `ps_num_fds()` returns the number of open files/handles.

* `ps_open_files()` lists all open files of a process.

* `ps_interrupt()` interrupts a process. It sends a `SIGINT` signal on
  POSIX systems, and it can send CTRL+C or CTRL+BREAK events on Windows.

* `ps_users()` lists users connected to the system.

* New functions to mark and clean up child processes: `ps_mark_tree()`,
  `ps_find_tree()`, `ps_kill_tree()`, `with_process_cleanup()`.

* New reporter `CleanupReporter`, to be used with testthat: it checks for
  leftover child processes an open files in `test_that()` blocks.

# ps 1.0.0

First released version.
