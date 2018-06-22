/*
 * Copyright (c) 2009, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

#include <Winsvc.h>

#include <Rinternals.h>

SC_HANDLE psutil_get_service_handle(
char service_name, DWORD scm_access, DWORD access);
SEXP psutil_winservice_enumerate(SEXP self, SEXP args);
SEXP psutil_winservice_query_config(SEXP self, SEXP args);
SEXP psutil_winservice_query_status(SEXP self, SEXP args);
SEXP psutil_winservice_query_descr(SEXP self, SEXP args);
SEXP psutil_winservice_start(SEXP self, SEXP args);
SEXP psutil_winservice_stop(SEXP self, SEXP args);
