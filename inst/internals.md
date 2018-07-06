
# `ps_handle` methods

```
method           A  C  Z
--------------   -  -  -
ps_pid           A  .  O
ps_create_time   A  .  O
ps_is_running    A  .  O
-
ps_ppid          .  A  O
ps_parent        .  A  O
ps_name          .  A  O
ps_exe           .  A  E
ps_cmdline       .  A  E
ps_status        .  A  O
ps_username      .  A  O
ps_cwd           .  A  E
ps_uids          .  A  O
ps_gids          .  A  O
ps_terminal      .  A  O
ps_environ       .  A  E
ps_environ_raw   .  A  E
ps_num_threads   .  A  E
ps_cpu_times     .  A  E
ps_memory_info   .  A  E
ps_send_signal   .  B  O
ps_suspend       .  B  O
ps_resume        .  B  O
ps_terminate     .  B  O
ps_kill          .  B  O
```

```
A: always works, even if the process has finished
C: B: checks if process is running, before
   A: checks if process is running, after
Z: O: works find on a zombie
   R: errors (zombie_process) on a zombie
```
