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

/* POSIX */
SEXP psp__pid_exists2(SEXP r_pid);
SEXP psp__get_pw_uid(SEXP r_uid);
SEXP psp__kill(SEXP r_pid, SEXP r_sig);
SEXP psp__stat_st_rdev(SEXP files);

/* MACOS */
SEXP psm__pids();
SEXP psm__proc_exe(SEXP r_pid);
SEXP psm__proc_cmdline(SEXP r_pid);
SEXP psm__proc_environ(SEXP r_pid);
SEXP psm__proc_cwd(SEXP r_pid);
SEXP psm__proc_kinfo_oneshot(SEXP r_pid);
SEXP psm__proc_pidtaskinfo_oneshot(SEXP r_pid);

/* WINDOWS */
SEXP psw__pids();
SEXP psw__ppid_map();
SEXP psw__pid_exists(SEXP r_pid);
SEXP psw__boot_time();
SEXP psw__proc_name(SEXP r_pid);
SEXP psw__proc_exe(SEXP r_pid);
SEXP psw__proc_cmdline(SEXP r_pid);
SEXP psw__proc_environ(SEXP r_pid);
SEXP psw__proc_cwd(SEXP r_pid);
SEXP psw__proc_username(SEXP r_pid);
SEXP psw__proc_info(SEXP r_pid);
SEXP psw__proc_memory_info(SEXP r_pid);
SEXP psw__proc_cpu_times(SEXP r_pid);
SEXP psw__proc_create_time(SEXP r_pid);
SEXP psw__proc_is_suspended(SEXP r_pid);
SEXP psw__proc_suspend(SEXP r_pid);
SEXP psw__proc_resume(SEXP r_pid);
SEXP psw__proc_kill(SEXP r_pid);
SEXP psw__win32_QueryDosDevice(SEXP r_path);

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
