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


/*
 * Return process name from kinfo_proc as a string.
 */
SEXP ps__proc_name(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  struct kinfo_proc kp;

  if (ps__get_kinfo_proc(pid, &kp) == -1) ps__throw_error();
  return ps__str_to_utf8(kp.kp_proc.p_comm);
}


/*
 * Return process current working directory.
 * Raises NSP in case of zombie process.
 */
SEXP ps__proc_cwd(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  struct proc_vnodepathinfo pathinfo;

  if (ps__proc_pidinfo(pid, PROC_PIDVNODEPATHINFO, 0, &pathinfo,
		       sizeof(pathinfo)) <= 0) {
    ps__throw_error();
  }

  return ps__str_to_utf8(pathinfo.pvi_cdir.vip_path);
}


/*
 * Return path of the process executable.
 */
SEXP ps__proc_exe(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  char buf[PATH_MAX];
  int ret;

  errno = 0;
  ret = proc_pidpath((pid_t)pid, &buf, sizeof(buf));
  if (ret == 0) {
    if (pid == 0)
      ps__access_denied("");
    else
      ps__raise_for_pid(pid, "proc_pidpath()");
    ps__throw_error();
  }
  return ps__str_to_utf8(buf);
}


/*
 * Return process cmdline as a character vector of cmdline arguments.
 */
SEXP ps__proc_cmdline(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];

  // get the commandline, defined in arch/macos/process_info.c
  return ps__get_cmdline(pid);
}


/*
 * Return process environment as a character vector.
 */
SEXP ps__proc_environ(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];

  // get the environment block, defined in arch/macos/process_info.c
  return ps__get_environ(pid);
}

/*
 * Return a list representing user, kernel and idle CPU times
 */
SEXP ps__cpu_times() {
  mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
  kern_return_t error;
  host_cpu_load_info_data_t r_load;

  mach_port_t host_port = mach_host_self();
  error = host_statistics(host_port, HOST_CPU_LOAD_INFO,
			  (host_info_t)&r_load, &count);
  if (error != KERN_SUCCESS) {
    ps__set_error("host_statistics(HOST_CPU_LOAD_INFO) syscall failed: %s",
		  mach_error_string(error));
    ps__throw_error();
  }
  mach_port_deallocate(mach_task_self(), host_port);

  return ps__build_list(
    "dddd",
    (double) r_load.cpu_ticks[CPU_STATE_USER] / CLK_TCK,
    (double) r_load.cpu_ticks[CPU_STATE_NICE] / CLK_TCK,
    (double) r_load.cpu_ticks[CPU_STATE_SYSTEM] / CLK_TCK,
    (double) r_load.cpu_ticks[CPU_STATE_IDLE] / CLK_TCK);
}


SEXP ps__init(SEXP psenv, SEXP constenv) {

  /* Signals */
  defineVar(install("signals"), ps__define_signals(), constenv);

  /* errno values */
  defineVar(install("errno"), ps__define_errno(), constenv);

  return R_NilValue;
}
