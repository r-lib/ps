
#include "config.h"

#ifndef PS__POSIX
void psp__pid_exists2() { }
void psp__get_pw_uid() { }
void psp__kill() { }
void psp__stat_st_rdev() { }
#endif

#ifndef PS__MACOS
void psm__pids() { }
void psm__proc_exe() { }
void psm__proc_cmdline() { }
void psm__proc_environ() { }
void psm__proc_cwd() { }
void psm__proc_name() { }
void psm__proc_kinfo_oneshot() { }
void psm__proc_pidtaskinfo_oneshot() { }
#endif

#ifndef PS__LINUX
void psl__readlink() { }
void psl__linux_clk_tck() { }
void psl__linux_pagesize() { }
void psl__parse_stat_file() { }
void psl__linux_parse_environ() { }
void psl__linux_match_environ() { }
void psl__kill_tree_process() { }
#endif

#ifndef PS__WINDOWS
void psw__pids() { }
void psw__proc_exe() { }
void psw__proc_cmdline() { }
void psw__proc_environ() { }
void psw__proc_cwd() { }
void psw__proc_name() { }
void psw__pid_exists() { }
void psw__ppid_map() { }
void psw__boot_time() { }
void psw__proc_username() { }
void psw__proc_info() { }
void psw__proc_memory_info() { }
void psw__proc_cpu_times() { }
void psw__proc_create_time() { }
void psw__proc_is_suspended() { }
void psw__proc_suspend() { }
void psw__proc_resume() { }
void psw__proc_kill() { }
void psw__win32_QueryDosDevice() { }
void psw__kill_tree_process() { }
#endif
