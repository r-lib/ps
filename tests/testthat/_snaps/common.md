# kill 2

    Code
      ps_kill(list(ph5, ph6))
    Condition
      Error:
      ! preventing sending KILL signal to process with PID 0 as it would affect every process in the process group of the calling process (Sys.getpid()) instead of PID 0

---

    Code
      ps_kill(list(ph7, ph8, ph9))
    Condition
      Error:
      ! Failed to kill some processes: 1 (launchd)

