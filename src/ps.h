
#ifndef R_PS_H
#define R_PS_H

#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>

/* API to be used from R */

/* ps_handle class */

SEXP psll_handle(SEXP pid, SEXP time);

SEXP psll_pid(SEXP p);
SEXP psll_create_time(SEXP p);

SEXP psll_format(SEXP p);
SEXP psll_parent(SEXP p);
SEXP psll_ppid(SEXP p);
SEXP psll_is_running(SEXP p);
SEXP psll_name(SEXP p);
SEXP psll_exe(SEXP p);
SEXP psll_cmdline(SEXP p);
SEXP psll_status(SEXP p);
SEXP psll_username(SEXP p);
SEXP psll_cwd(SEXP p);
SEXP psll_uids(SEXP p);
SEXP psll_gids(SEXP p);
SEXP psll_terminal(SEXP p);
SEXP psll_environ(SEXP p);
SEXP psll_num_threads(SEXP p);
SEXP psll_cpu_times(SEXP p);
SEXP psll_memory_info(SEXP p);
SEXP psll_send_signal(SEXP p, SEXP sig);
SEXP psll_suspend(SEXP p);
SEXP psll_resume(SEXP p);
SEXP psll_terminate(SEXP p);
SEXP psll_kill(SEXP p);
SEXP psll_num_fds(SEXP p);
SEXP psll_open_files(SEXP p);
SEXP psll_interrupt(SEXP p, SEXP ctrlc, SEXP interrupt_path);
SEXP psll_connections(SEXP p);

/* System API */

SEXP ps__os_type();
SEXP ps__pids();
SEXP ps__boot_time();
SEXP ps__users();

/* Generic utils used from R */

SEXP ps__init(SEXP psenv, SEXP constenv);
SEXP ps__kill_if_env(SEXP marker, SEXP after, SEXP pid, SEXP sig);
SEXP ps__find_if_env(SEXP marker, SEXP after, SEXP pid);
SEXP ps__inet_ntop(SEXP raw, SEXP fam);

SEXP psp__zombie();
SEXP psp__waitpid(SEXP pid);
SEXP psp__pid_exists(SEXP r_pid);
SEXP psp__stat_st_rdev(SEXP files);

SEXP psw__realpath(SEXP path);
#endif
