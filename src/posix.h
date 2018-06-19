/*
 * Copyright (c) 2009, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

int ps__pid_exists(long pid);
void ps__raise_for_pid(long pid, char *msg);
SEXP ps__posix_getpriority(SEXP r_pid);
SEXP ps__posix_setpriority(SEXP r_pid, SEXP r_priority);
