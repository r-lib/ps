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

SEXP ps__pids();
SEXP ps__pid_exists2(SEXP r_pid);
SEXP ps__proc_exe(SEXP r_pid);
SEXP ps__proc_cmdline(SEXP r_pid);
SEXP ps__proc_environ(SEXP r_pid);
SEXP ps__proc_cwd(SEXP r_pid);

#ifdef PS__OSX
SEXP ps__proc_kinfo_oneshot(SEXP r_pid);
SEXP ps__proc_pidtaskinfo_oneshot(SEXP r_pid);
#endif

/* ---------------------------------------------------------------------*/
/* Internals                                                            */
/* ---------------------------------------------------------------------*/

extern int PS__TESTING;
extern int PS__DEBUG;

// a signaler for connections without an actual status
static const int PS__CONN_NONE = 128;

const char *ps__get_error();
void ps__set_error(const char *msg, ...);

void ps__access_denied(const char *msg);
void ps__no_such_process(const char *msg);
void ps__zombie_process(const char *msg);

void ps__set_testing();
void ps__debug(const char* format, ...);
void R_init_ps(DllInfo *dll);

#endif // PSUTIL_PSUTIL_COMMON_H
