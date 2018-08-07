
#include "config.h"
#include <Rinternals.h>

void *ps__not_implemented(const char *what);
SEXP ps__throw_error();

void ps__dummy(const char *what) {
  ps__not_implemented(what);
  ps__throw_error();
}

#ifdef  PS__LINUX
#ifndef PS__MACOS
#ifndef PS__WINDOWS
void ps__pids()          { ps__dummy("ps_pids"); }
#endif
#endif
#endif

#ifdef PS__WINDOWS
#ifndef PS__POSIX
void psp__pid_exists()   { ps__dummy("psp__pid_exists"); }
void psp__zombie()       { ps__dummy("psp__zombie"); }
void psp__waitpid()      { ps__dummy("psp__waitpid"); }
void psp__stat_st_rdev() { ps__dummy("psp__stat_st_rdev"); }
#endif
#endif

#ifdef PS__POSIX
#ifndef PS__WINDOWS
void psw__realpath()     { ps__dummy("psw__realpath"); }
#endif
#endif

#ifndef PS__MACOS
#ifndef PS__LINUX
#ifndef PS__WINDOWS
void ps__pids()          { ps__dummy("ps_pids"); }
void ps__boot_time()     { ps__dummy("ps_boot_time"); }
void ps__users()         { ps__users("ps_users"); }

void psll_handle()       { ps__dummy("ps_handle"); }
void psll_format()       { ps__dummy("ps_format"); }
void psll_parent()       { ps__dummy("ps_handle"); }
void psll_ppid()         { ps__dummy("ps_handle"); }
void psll_is_running()   { ps__dummy("ps_is_running"); }
void psll_name()         { ps__dummy("ps_name"); }
void psll_exe()          { ps__dummy("ps_exe"); }
void psll_cmdline()      { ps__dummy("ps_cmdline"); }
void psll_status()       { ps__dummy("ps_status"); }
void psll_username()     { ps__dummy("ps_username"); }
void psll_cwd()          { ps__dummy("ps_cwd"); }
void psll_uids()         { ps__dummy("ps_uids"); }
void psll_gids()         { ps__dummy("ps_gids"); }
void psll_terminal()     { ps__dummy("ps_terminal"); }
void psll_environ()      { ps__dummy("ps_environ"); }
void psll_num_threads()  { ps__dummy("ps_num_threads"); }
void psll_cpu_times()    { ps__dummy("ps_cpu_times"); }
void psll_memory_info()  { ps__dummy("ps_memory_info"); }
void psll_send_signal()  { ps__dummy("ps_send_signal"); }
void psll_suspend()      { ps__dummy("ps_suspend"); }
void psll_resume()       { ps__dummy("ps_resume"); }
void psll_terminate()    { ps__dummy("ps_terminate"); }
void psll_kill()         { ps__dummy("ps_kill"); }
void psll_num_fds()      { ps__dummy("ps_num_fds"); }
void psll_open_files()   { ps__dummy("ps_open_files"); }
void psll_interrupt()    { ps__dummy("ps_interrupt"); }

void ps__init()          { /* this needs to run to load package */ }
void ps__kill_if_env()   { ps__dummy("ps__kill_if_env"); }
void ps__find_if_env()   { ps__dummy("ps__find_if_env"); }

void psp__pid_exists()   { ps__dummy("psp__pid_exists"); }
void psp__stat_st_rdev() { ps__dummy("psp__stat_st_rdev"); }
void psp__zombie()       { ps__dummy("psp__zombie"); }
void psp__waitpid()      { ps__dummy("psp__waitpid"); }

void psw__realpath()     { ps__dummy("psw__realpath"); }
#endif
#endif
#endif
