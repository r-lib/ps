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

// Global vars.
int PS__DEBUG = 0;
int PS__TESTING = 0;

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
