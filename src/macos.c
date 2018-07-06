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
 * A wrapper around host_statistics() invoked with HOST_VM_INFO.
 */
int ps__sys_vminfo(vm_statistics_data_t *vmstat) {
  kern_return_t ret;
  mach_msg_type_number_t count = sizeof(*vmstat) / sizeof(integer_t);
  mach_port_t mport = mach_host_self();

  ret = host_statistics(mport, HOST_VM_INFO, (host_info_t)vmstat, &count);
  if (ret != KERN_SUCCESS) {
    ps__set_error("host_statistics(HOST_VM_INFO) syscall failed: %s",
		  mach_error_string(ret));
    return 0;
  }
  mach_port_deallocate(mach_task_self(), mport);
  return 1;
}


/*
 * Return 1 if pid refers to a zombie process else 0.
 */
int ps__is_zombie(long pid)
{
  struct kinfo_proc kp;

  if (ps__get_kinfo_proc(pid, &kp) == -1)
    return 0;
  return (kp.kp_proc.p_stat == SZOMB) ? 1 : 0;
}


/*
 * A wrapper around task_for_pid() which sucks big time:
 * - it's not documented
 * - errno is set only sometimes
 * - sometimes errno is ENOENT (?!?)
 * - for PIDs != getpid() or PIDs which are not members of the procmod
 *   it requires root
 * As such we can only guess what the heck went wrong and fail either
 * with NoSuchProcess, ZombieProcessError or giveup with AccessDenied.
 * Here's some history:
 * https://github.com/giampaolo/psutil/issues/1181
 * https://github.com/giampaolo/psutil/issues/1209
 * https://github.com/giampaolo/psutil/issues/1291#issuecomment-396062519
 */
int ps__task_for_pid(long pid, mach_port_t *task)
{
  // See: https://github.com/giampaolo/psutil/issues/1181
  kern_return_t err = KERN_SUCCESS;

  err = task_for_pid(mach_task_self(), (pid_t)pid, task);
  if (err != KERN_SUCCESS) {
    if (ps__pid_exists(pid) == 0)
      ps__no_such_process(pid, 0);
    else if (ps__is_zombie(pid) == 1)
      ps__zombie_process(pid);
    else {
      ps__debug("task_for_pid() failed (pid=%ld, err=%i, errno=%i, msg='%s'); "
		"setting AccessDenied()",
		pid, err, errno, mach_error_string(err));
      ps__access_denied("task_for_pid() failed");
    }
    return 1;
  }
  return 0;
}


/*
 * Return an integer vector of all the PIDs running on the system.
 */
SEXP psm__pids() {
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
 * Return multiple process info as a list in one shot by
 * using sysctl() and filling up a kinfo_proc struct.
 * It should be possible to do this for all processes without
 * incurring into permission (EPERM) errors.
 * This will also succeed for zombie processes returning correct
 * information.
 */
SEXP psm__proc_kinfo_oneshot(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];;
  struct kinfo_proc kp;
  SEXP name = R_NilValue;
  SEXP retlist = R_NilValue;

  if (ps__get_kinfo_proc(pid, &kp) == -1) ps__throw_error();

  /* Will be NA if cannot be converted */
  name = PROTECT(ps__str_to_utf8(kp.kp_proc.p_comm));

  retlist = PROTECT(ps__build_named_list(
    "lllllllidiO",
    "ppid",   (long)   kp.kp_eproc.e_ppid,
    "ruid",   (long)   kp.kp_eproc.e_pcred.p_ruid,
    "euid",   (long)   kp.kp_eproc.e_ucred.cr_uid,
    "suid",   (long)   kp.kp_eproc.e_pcred.p_svuid,
    "rgid",   (long)   kp.kp_eproc.e_pcred.p_rgid,
    "egid",   (long)   kp.kp_eproc.e_ucred.cr_groups[0],
    "sgid",   (long)   kp.kp_eproc.e_pcred.p_svgid,
    "ttynr",  (int)    kp.kp_eproc.e_tdev,
    "ctime",  (double) PS__TV2DOUBLE(kp.kp_proc.p_starttime),
    "status", (int)    kp.kp_proc.p_stat,
    "name",            name));

  UNPROTECT(2);
  return retlist;
}


/*
 * Return multiple process info as list in one shot by
 * using proc_pidinfo(PROC_PIDTASKINFO) and filling a proc_taskinfo
 * struct.
 * Contrarily from proc_kinfo above this function will fail with
 * EACCES for PIDs owned by another user and with ESRCH for zombie
 * processes.
 */
SEXP psm__proc_pidtaskinfo_oneshot(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  struct proc_taskinfo pti;

  if (ps__proc_pidinfo(pid, PROC_PIDTASKINFO, 0, &pti, sizeof(pti)) <= 0)
    ps__throw_error();

  return ps__build_named_list(
    "ddKKkkkk",
    "cpuutime",   (double) pti.pti_total_user / 1000000000.0,
    "cpustime",   (double) pti.pti_total_system / 1000000000.0,
    // Note about memory: determining other mem stats on MACOS is a mess:
    // http://www.opensource.apple.com/source/top/top-67/libtop.c?txt
    // I just give up.
    // struct proc_regioninfo pri;
    // psutil_proc_pidinfo(pid, PROC_PIDREGIONINFO, 0, &pri, sizeof(pri))
    "rss",        (unsigned long long) pti.pti_resident_size,
    "vms",        (unsigned long long) pti.pti_virtual_size,
    "pfaults",    (unsigned long)      pti.pti_faults,
    "pageins",    (unsigned long)      pti.pti_pageins,
    "numthreads", (unsigned long)      pti.pti_threadnum,
    // Unvoluntary value seems not to be available;
    // pti.pti_csw probably refers to the sum of the two;
    // getrusage() numbers seems to confirm this theory.
    "volctxsw",   (unsigned long)      pti.pti_csw);
}


/*
 * Return process name from kinfo_proc as a string.
 */
SEXP psm__proc_name(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  struct kinfo_proc kp;

  if (ps__get_kinfo_proc(pid, &kp) == -1) ps__throw_error();
  return ps__str_to_utf8(kp.kp_proc.p_comm);
}


/*
 * Return process current working directory.
 * Raises NSP in case of zombie process.
 */
SEXP psm__proc_cwd(SEXP r_pid) {
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
SEXP psm__proc_exe(SEXP r_pid) {
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
SEXP psm__proc_cmdline(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];

  // get the commandline, defined in arch/macos/process_info.c
  return ps__get_cmdline(pid);
}


/*
 * Return process environment as a character vector.
 */
SEXP psm__proc_environ(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];

  // get the environment block, defined in arch/macos/process_info.c
  return ps__get_environ(pid);
}


static void ps__proc_memory_maps_free(SEXP ptr) {
  mach_port_t *task = R_ExternalPtrAddr(ptr);
  if (!task) return;
  mach_port_deallocate(mach_task_self(), *task);
}

/*
 * Return a list of tuples for every process memory maps.
 * 'procstat' cmdline utility has been used as an example.
 */
SEXP ps__proc_memory_maps(SEXP r_pid) {
  char buf[PATH_MAX];
  char addr_str[34];
  char perms[8];
  int pagesize = getpagesize();
  long pid = INTEGER(r_pid)[0];
  kern_return_t err = KERN_SUCCESS;
  mach_port_t *task = malloc(sizeof(mach_port_t));
  uint32_t depth = 1;
  vm_address_t address = 0;
  vm_size_t size = 0;

  SEXP rtask = R_NilValue;
  SEXP tuple = R_NilValue;
  SEXP path = R_NilValue;
  SEXP list = R_NilValue;
  PROTECT_INDEX ipx;

  if  (!task) {
    ps__set_error_from_errno();
    ps__throw_error();
  }
  *task = MACH_PORT_NULL;

  PROTECT_WITH_INDEX(list = allocVector(VECSXP, 0), &ipx);

  if (ps__task_for_pid(pid, task) != 0)
    ps__throw_error();

  rtask = PROTECT(R_MakeExternalPtr(task, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(rtask, ps__proc_memory_maps_free, 0);

  while (1) {
    struct vm_region_submap_info_64 info;
    mach_msg_type_number_t count = VM_REGION_SUBMAP_INFO_COUNT_64;

    err = vm_region_recurse_64(*task, &address, &size, &depth,
			       (vm_region_info_64_t)&info, &count);
    if (err == KERN_INVALID_ADDRESS) {
      // TODO temporary
      ps__debug("vm_region_recurse_64 returned KERN_INVALID_ADDRESS");
      break;
    }
    if (err != KERN_SUCCESS) {
      ps__debug("vm_region_recurse_64 returned !=  KERN_SUCCESS");
    }

    if (info.is_submap) {
      depth++;
    }
    else {
      // Free/Reset the char[]s to avoid weird paths
      memset(buf, 0, sizeof(buf));
      memset(addr_str, 0, sizeof(addr_str));
      memset(perms, 0, sizeof(perms));

      sprintf(addr_str,
	      "%016lx-%016lx",
	      (long unsigned int)address,
	      (long unsigned int)address + size);
      sprintf(perms, "%c%c%c/%c%c%c",
	      (info.protection & VM_PROT_READ) ? 'r' : '-',
	      (info.protection & VM_PROT_WRITE) ? 'w' : '-',
	      (info.protection & VM_PROT_EXECUTE) ? 'x' : '-',
	      (info.max_protection & VM_PROT_READ) ? 'r' : '-',
	      (info.max_protection & VM_PROT_WRITE) ? 'w' : '-',
	      (info.max_protection & VM_PROT_EXECUTE) ? 'x' : '-');

      // proc_regionfilename() return value seems meaningless
      // so we do what we can in order to not continue in case
      // of error.
      errno = 0;
      proc_regionfilename((pid_t)pid, address, buf, sizeof(buf));
      if ((errno != 0) || ((sizeof(buf)) <= 0)) {
	// TODO temporary
	ps__debug("proc_regionfilename() failed");
	ps__raise_for_pid(pid, "proc_regionfilename()");
	ps__throw_error();
      }

      if (info.share_mode == SM_COW && info.ref_count == 1) {
	// Treat single reference SM_COW as SM_PRIVATE
	info.share_mode = SM_PRIVATE;
      }

      if (strlen(buf) == 0) {
	switch (info.share_mode) {
	  // #ifdef SM_LARGE_PAGE
	  // case SM_LARGE_PAGE:
	  // Treat SM_LARGE_PAGE the same as SM_PRIVATE
	  // since they are not shareable and are wired.
	  // #endif
	case SM_COW:
	  strcpy(buf, "[cow]");
	  break;
	case SM_PRIVATE:
	  strcpy(buf, "[prv]");
	  break;
	case SM_EMPTY:
	  strcpy(buf, "[nul]");
	  break;
	case SM_SHARED:
	case SM_TRUESHARED:
	  strcpy(buf, "[shm]");
	  break;
	case SM_PRIVATE_ALIASED:
	  strcpy(buf, "[ali]");
	  break;
	case SM_SHARED_ALIASED:
	  strcpy(buf, "[s/a]");
	  break;
	default:
	  strcpy(buf, "[???]");
	}
      }

      path = PROTECT(ps__str_to_utf8(buf));
      tuple = ps__build_named_list(
	"ssOIIIIIH",
        "start_end_address", addr_str,
	"permissions",       perms,
	"path",              path,
	"rss",               info.pages_resident * pagesize,
	"private",           info.pages_shared_now_private * pagesize,
	"swapped",           info.pages_swapped_out * pagesize,
	"dirtied",           info.pages_dirtied * pagesize,
	"ref_count",         info.ref_count,
	"shadow_depth",      info.shadow_depth);

      UNPROTECT(1);
      REPROTECT(list = Rf_listAppend(list, tuple), ipx);
    }

    // increment address for the next map/file
    address += size;
  }

  UNPROTECT(2);
  return list;
}


/*
 * Return the number of logical CPUs in the system.
 * XXX this could be shared with BSD.
 */
SEXP ps__cpu_count_logical() {
  /*
    int mib[2];
    int ncpu;
    size_t len;
    mib[0] = CTL_HW;
    mib[1] = HW_NCPU;
    len = sizeof(ncpu);

    if (sysctl(mib, 2, &ncpu, &len, NULL, 0) == -1)
    Py_RETURN_NONE;  // mimic os.cpu_count()
    else
    return Py_BuildValue("i", ncpu);
  */
  int num;
  size_t size = sizeof(int);

  if (sysctlbyname("hw.logicalcpu", &num, &size, NULL, 2))
    return R_NilValue;
  else
    return ScalarInteger(num);
}


/*
 * Return the number of physical CPUs in the system.
 */
SEXP ps__cpu_count_phys() {
  int num;
  size_t size = sizeof(int);

  if (sysctlbyname("hw.physicalcpu", &num, &size, NULL, 0))
    return R_NilValue;
  else
    return ScalarInteger(num);
}


/*
 * Indicates if the given virtual address on the given architecture is in the
 * shared VM region.
 */
bool
ps__in_shared_region(mach_vm_address_t addr, cpu_type_t type) {
  mach_vm_address_t base;
  mach_vm_address_t size;

  switch (type) {
  case CPU_TYPE_ARM:
    base = SHARED_REGION_BASE_ARM;
    size = SHARED_REGION_SIZE_ARM;
    break;
  case CPU_TYPE_I386:
    base = SHARED_REGION_BASE_I386;
    size = SHARED_REGION_SIZE_I386;
    break;
  case CPU_TYPE_X86_64:
    base = SHARED_REGION_BASE_X86_64;
    size = SHARED_REGION_SIZE_X86_64;
    break;
  default:
    return false;
  }

  return base <= addr && addr < (base + size);
}


/*
 * Returns the USS (unique set size) of the process. Reference:
 * https://dxr.mozilla.org/mozilla-central/source/xpcom/base/
 *     nsMemoryReporterManager.cpp
 */
SEXP ps__proc_memory_uss(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  size_t len;
  cpu_type_t cpu_type;
  size_t private_pages = 0;
  mach_vm_size_t size = 0;
  mach_msg_type_number_t info_count = VM_REGION_TOP_INFO_COUNT;
  kern_return_t kr;
  vm_size_t page_size;
  mach_vm_address_t addr = MACH_VM_MIN_ADDRESS;
  mach_port_t task = MACH_PORT_NULL;
  vm_region_top_info_data_t info;
  mach_port_t object_name;

  if (ps__task_for_pid(pid, &task) != 0)
    ps__throw_error();

  len = sizeof(cpu_type);
  if (sysctlbyname("sysctl.proc_cputype", &cpu_type, &len, NULL, 0) != 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  // Roughly based on libtop_update_vm_regions in
  // http://www.opensource.apple.com/source/top/top-100.1.2/libtop.c
  for (addr = 0; ; addr += size) {
    kr = mach_vm_region(
			task, &addr, &size, VM_REGION_TOP_INFO, (vm_region_info_t)&info,
			&info_count, &object_name);
    if (kr == KERN_INVALID_ADDRESS) {
      // Done iterating VM regions.
      break;
    }
    else if (kr != KERN_SUCCESS) {
      ps__set_error("mach_vm_region(VM_REGION_TOP_INFO) syscall failed");
      ps__throw_error();
    }

    if (ps__in_shared_region(addr, cpu_type) &&
	info.share_mode != SM_PRIVATE) {
      continue;
    }

    switch (info.share_mode) {
#ifdef SM_LARGE_PAGE
    case SM_LARGE_PAGE:
      // NB: Large pages are not shareable and always resident.
#endif
    case SM_PRIVATE:
      private_pages += info.private_pages_resident;
      private_pages += info.shared_pages_resident;
      break;
    case SM_COW:
      private_pages += info.private_pages_resident;
      if (info.ref_count == 1) {
	// Treat copy-on-write pages as private if they only
	// have one reference.
	private_pages += info.shared_pages_resident;
      }
      break;
    case SM_SHARED:
    default:
      break;
    }
  }

  mach_port_deallocate(mach_task_self(), task);

  if (host_page_size(mach_host_self(), &page_size) != KERN_SUCCESS)
    page_size = PAGE_SIZE;

  return ScalarReal(private_pages * page_size);
}


/*
 * Return system virtual memory stats.
 * See:
 * http://opensource.apple.com/source/system_cmds/system_cmds-498.2/
 *     vm_stat.tproj/vm_stat.c
 */
SEXP ps__virtual_mem() {
  int      mib[2];
  uint64_t total;
  size_t   len = sizeof(total);
  vm_statistics_data_t vm;
  int pagesize = getpagesize();
  // physical mem
  mib[0] = CTL_HW;
  mib[1] = HW_MEMSIZE;

  // This is also available as sysctlbyname("hw.memsize").
  if (sysctl(mib, 2, &total, &len, NULL, 0)) {
    if (errno != 0)
      ps__set_error_from_errno();
    else
      ps__set_error("sysctl(HW_MEMSIZE) syscall failed");
    ps__throw_error();
  }

  // vm
  if (!ps__sys_vminfo(&vm))
    ps__throw_error();

  return ps__build_list(
    "KKKKK",
    total,
    (unsigned long long) vm.active_count * pagesize,
    (unsigned long long) vm.inactive_count * pagesize,
    (unsigned long long) vm.wire_count * pagesize,
    // this is how vm_stat cmd does it
    (unsigned long long) (vm.free_count - vm.speculative_count) * pagesize);
}


/*
 * Return stats about swap memory.
 */
SEXP ps__swap_mem() {
  int mib[2];
  size_t size;
  struct xsw_usage totals;
  vm_statistics_data_t vmstat;
  int pagesize = getpagesize();

  mib[0] = CTL_VM;
  mib[1] = VM_SWAPUSAGE;
  size = sizeof(totals);
  if (sysctl(mib, 2, &totals, &size, NULL, 0) == -1) {
    if (errno != 0)
      ps__set_error_from_errno();
    else
      ps__set_error("sysctl(VM_SWAPUSAGE) syscall failed");
    ps__throw_error();
  }
  if (!ps__sys_vminfo(&vmstat))
    ps__throw_error();

  return ps__build_list(
    "LLLKK",
    totals.xsu_total,
    totals.xsu_used,
    totals.xsu_avail,
    (unsigned long long)vmstat.pageins * pagesize,
    (unsigned long long)vmstat.pageouts * pagesize);
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

  // process status constants, defined in:
  // http://fxr.watson.org/fxr/source/bsd/sys/proc.h?v=xnu-792.6.70#L149
  defineVar(install("SIDL"),   ScalarInteger(SIDL),  constenv);
  defineVar(install("SRUN"),   ScalarInteger(SRUN),   constenv);
  defineVar(install("SSLEEP"), ScalarInteger(SSLEEP), constenv);
  defineVar(install("SSTOP"),  ScalarInteger(SSTOP),  constenv);
  defineVar(install("SZOMB"),  ScalarInteger(SZOMB),  constenv);

  /* Signals */
  defineVar(install("signals"), psp__define_signals(), constenv);

  /* errno values */
  defineVar(install("errno"), psp__define_errno(), constenv);

  return R_NilValue;
}
