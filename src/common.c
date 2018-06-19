/*
 * Copyright (c) 2009, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *
 * Routines common to all platforms.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "common.h"
#include "posix.h"

// Global vars.
int PS__DEBUG = 0;
int PS__TESTING = 0;

char ps__last_error[500];

/* TODO: these should throw real error objects */

const char *ps__get_error() {
  return ps__last_error;
}

void ps__set_error(const char *msg, ...) {
  va_list args;
  va_start(args, msg);
  vsnprintf(ps__last_error, sizeof(ps__last_error) - 1, msg, args);
  va_end(args);
}

void ps__no_such_process(const char *msg) {
  ps__set_error(msg && strlen(msg) ? msg : "No such process");
}

void ps__access_denied(const char *msg) {
  ps__set_error(msg && strlen(msg) ? msg : "Permission denied");
}

void ps__zombie_process(const char *msg) {
  ps__set_error(msg && strlen(msg) ? msg : "Process is a zombie");
}

/*
 * Enable testing mode. This has the same effect as setting PS__TESTING
 * env var. This dual method exists because updating os.environ on
 * Windows has no effect. Called on unit tests setup.
 */
void ps__set_testing() {
  PS__TESTING = 1;
}


/*
 * Print a debug message on stderr. No-op if PS__DEBUG env var is not set.
 */
void ps__debug(const char* format, ...) {
  va_list argptr;
  va_start(argptr, format);
  REprintf("psutil-debug> ");
  REvprintf(format, argptr);
  REprintf("\n");
  va_end(argptr);
}

static const R_CallMethodDef callMethods[]  = {
  { "ps__pids",       (DL_FUNC) ps__pids,       0 },
  { "ps__pid_exists", (DL_FUNC) ps__pid_exists2, 1 },

  { NULL, NULL, 0 }
};

/*
 * Called on module import on all platforms.
 */
void R_init_ps(DllInfo *dll) {
  if (getenv("R_PS_DEBUG") != NULL) PS__DEBUG = 1;
  if (getenv("R_PS_TESTING") != NULL) PS__TESTING = 1;

  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
