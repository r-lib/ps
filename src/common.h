/*
 * Copyright (c) 2009, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

#ifndef R_PS_COMMON_H
#define R_PS_COMMON_H

#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "config.h"
#include "extra.h"

/* ---------------------------------------------------------------------*/
/* API from R                                                           */
/* ---------------------------------------------------------------------*/

#ifdef PS__POSIX
SEXP ps__pid_exists2(SEXP r_pid);
SEXP ps__get_pw_uid(SEXP r_uid);
SEXP ps__kill(SEXP r_pid, SEXP r_sig);
SEXP ps__stat_st_rdev(SEXP files);
#endif

#ifdef PS__OSX
SEXP ps__pids();
SEXP ps__proc_exe(SEXP r_pid);
SEXP ps__proc_cmdline(SEXP r_pid);
SEXP ps__proc_environ(SEXP r_pid);
SEXP ps__proc_cwd(SEXP r_pid);
SEXP ps__proc_kinfo_oneshot(SEXP r_pid);
SEXP ps__proc_pidtaskinfo_oneshot(SEXP r_pid);
#endif

#ifdef PS__WINDOWS
SEXP ps__pids();
SEXP ps__ppid_map();
SEXP ps__pid_exists(SEXP r_pid);
SEXP ps__boot_time();
SEXP ps__proc_name(SEXP r_pid);
SEXP ps__proc_exe(SEXP r_pid);
SEXP ps__proc_cmdline(SEXP r_pid);
SEXP ps__proc_environ(SEXP r_pid);
SEXP ps__proc_cwd(SEXP r_pid);
SEXP ps__proc_username(SEXP r_pid);
SEXP ps__proc_info(SEXP r_pid);
SEXP ps__proc_memory_info(SEXP r_pid);
SEXP ps__proc_cpu_times(SEXP r_pid);
SEXP ps__proc_create_time(SEXP r_pid);
SEXP ps__proc_is_suspended(SEXP r_pid);
SEXP ps__proc_suspend(SEXP r_pid);
SEXP ps__proc_resume(SEXP r_pid);
SEXP ps__proc_kill(SEXP r_pid);
SEXP ps__win32_QueryDosDevice(SEXP r_path);
#endif

/* ---------------------------------------------------------------------*/
/* Internals                                                            */
/* ---------------------------------------------------------------------*/

extern int PS__TESTING;
extern int PS__DEBUG;

// a signaler for connections without an actual status
static const int PS__CONN_NONE = 128;

void ps__set_testing();
void ps__debug(const char* format, ...);
void R_init_ps(DllInfo *dll);

#endif // PSUTIL_PSUTIL_COMMON_H
