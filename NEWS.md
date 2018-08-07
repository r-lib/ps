
# ps 1.0.0.9000

* `ps_num_fds()` returns the number of open files/handles.

* `ps_open_files()` lists all open files of a process.

* `ps_interrupt()` interrupts a process. It sends a `SIGINT` signal on
  POSIX systems, and it can send CTRL+C or CTRL+BREAK events on Windows.

* `ps_users()` lists users connected to the system.

# ps 1.0.0

First released version.
