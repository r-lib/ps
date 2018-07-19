/*
 * Copyright (c) 2009, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *
 * OS X platform-specific module methods for macos
 */

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <utmpx.h>
#include <sys/sysctl.h>
#include <sys/vmmeter.h>
#include <libproc.h>
#include <sys/proc_info.h>
#include <netinet/tcp_fsm.h>
#include <arpa/inet.h>
#include <net/if_dl.h>
#include <pwd.h>

#include <mach/mach.h>
#include <mach/task.h>
#include <mach/mach_init.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>
#include <mach/mach_traps.h>
#include <mach/mach_vm.h>
#include <mach/shared_region.h>

#include <mach-o/loader.h>

#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/storage/IOBlockStorageDriver.h>
#include <IOKit/storage/IOMedia.h>
#include <IOKit/IOBSD.h>
#include <IOKit/ps/IOPowerSources.h>
#include <IOKit/ps/IOPSKeys.h>

#include "common.h"
#include "posix.h"
#include "arch/macos/process_info.h"


#define PS__TV2DOUBLE(t) ((t).tv_sec + (t).tv_usec / 1000000.0)

/*
 * Return an integer vector of all the PIDs running on the system.
 */
SEXP ps__pids() {
  kinfo_proc *proclist = NULL;
  kinfo_proc *orig_address = NULL;
  size_t num_processes;
  size_t idx;
  SEXP retlist = R_NilValue;

  if (ps__get_proc_list(&proclist, &num_processes) != 0) {
    if (errno != 0) {
      ps__set_error_from_errno();
    } else {
      ps__set_error("failed to retrieve process list");
    }
    goto error;
  }

  retlist = PROTECT(allocVector(INTSXP, num_processes));

  if (num_processes > 0) {
    // save the address of proclist so we can free it later
    orig_address = proclist;
    for (idx = 0; idx < num_processes; idx++) {
      INTEGER(retlist)[idx] = proclist->kp_proc.p_pid;
      proclist++;
    }
    free(orig_address);
  }

  UNPROTECT(1);
  return retlist;

 error:
  if (orig_address != NULL) free(orig_address);
  ps__throw_error();
  return R_NilValue;
}

SEXP ps__init(SEXP psenv, SEXP constenv) {

  /* Signals */
  defineVar(install("signals"), ps__define_signals(), constenv);

  /* errno values */
  defineVar(install("errno"), ps__define_errno(), constenv);

  return R_NilValue;
}
