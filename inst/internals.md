
# `ps_handle` methods

```
method           A  C
--------------   -  -
ps_pid           A  .
ps_create_time   A  .
ps_is_running    A  .
-
ps_ppid          .  A
ps_parent        .  A
ps_name          .  A
ps_exe           .  A
ps_cmdline       .  A
ps_status        .  A
ps_username      .  A
ps_cwd           .  A
ps_uids          .  A
ps_gids          .  A
ps_terminal      .  A
ps_environ       .  A
ps_environ_raw   .  A
ps_num_threads   .  A
ps_cpu_times     .  A
ps_memory_info   .  A
ps_send_signal   .  B
ps_suspend       .  B
ps_resume        .  B
ps_terminate     .  B
ps_kill          .  B
```

```
A: always works, even if the process has finished
C: B: checks if process is running, before
   A: checks if process is running, after
```
