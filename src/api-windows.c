
#include "common.h"
#include "windows.h"
#include "arch/windows/process_info.h"
#include "arch/windows/process_handles.h"

#include <tlhelp32.h>
#include <string.h>
#include <math.h>
#include <wtsapi32.h>

static void psll_finalizer(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (handle) free(handle);
}

static int ps__create_time_raw(DWORD pid, FILETIME *ftCreate) {
  HANDLE hProcess = ps__handle_from_pid(pid);
  FILETIME ftExit, ftKernel, ftUser;

  if (! hProcess) goto error;	/* error set already */

  if (! GetProcessTimes(hProcess, ftCreate, &ftExit, &ftKernel, &ftUser)) {
    if (GetLastError() == ERROR_ACCESS_DENIED) {
      // usually means the process has died so we throw a
      // NoSuchProcess here
      ps__no_such_process(pid, 0);
    } else {
      ps__set_error_from_windows_error(0);
    }
    goto error;
  }

  CloseHandle(hProcess);
  return 0;

 error:
  if (hProcess) CloseHandle(hProcess);
  return -1;
}

double ps__filetime_to_unix(FILETIME ft) {
  long long ll, secs, nsecs;
  ll = ((LONGLONG) ft.dwHighDateTime) << 32;
  ll += ft.dwLowDateTime - 116444736000000000LL;
  secs = ll / 10000000;
  nsecs = ll % 10000000;
  return (double) secs + ((double) nsecs) / 10000000;
}

static SEXP ps__is_running(ps_handle_t *handle) {
  FILETIME ftCreate;
  int ret = ps__create_time_raw(handle->pid, &ftCreate);

  if (ret) return ScalarLogical(0);

  if (handle->wtime.dwHighDateTime) {
    if (handle->wtime.dwHighDateTime != ftCreate.dwHighDateTime ||
	handle->wtime.dwLowDateTime != ftCreate.dwLowDateTime)  {
      return ScalarLogical(0);
    } else {
      return ScalarLogical(1);
    }
  } else {
    double unix_time = ps__filetime_to_unix(ftCreate);
    if (fabs(unix_time - handle->create_time) > 0.01) {
      return ScalarLogical(0);
    } else  {
      return ScalarLogical(1);
    }
  }

  /* Never reached */
  return R_NilValue;
}

void PS__CHECK_HANDLE(ps_handle_t *handle) {
  SEXP ret = ps__is_running(handle);
  if (!LOGICAL(ret)[0]) {
    ps__no_such_process(handle->pid, 0);
    ps__throw_error();
  }
}

static int ps__proc_is_suspended(DWORD pid) {
  ULONG i;
  PSYSTEM_PROCESS_INFORMATION process;
  PVOID buffer;

  if (! ps__get_proc_info(pid, &process, &buffer)) return -1;

  for (i = 0; i < process->NumberOfThreads; i++) {
    if (process->Threads[i].ThreadState != Waiting ||
	process->Threads[i].WaitReason != Suspended) {
      free(buffer);
      return 0;
    }
  }
  free(buffer);
  return 1;
}

static SEXP ps__status(DWORD pid) {
  int ret = ps__proc_is_suspended(pid);
  if (ret < 0) return R_NilValue;

  return ret ? mkString("stopped") : mkString("running");
}

SEXP psll_handle(SEXP pid, SEXP time) {
  DWORD cpid = isNull(pid) ? GetCurrentProcessId() : INTEGER(pid)[0];
  double ctime;
  ps_handle_t *handle;
  SEXP res;
  FILETIME ftCreate = { 0, 0 };

  if (!isNull(time)) {
    ctime = REAL(time)[0];

  } else {
    int ret = ps__create_time_raw(cpid, &ftCreate);
    if (ret) ps__throw_error();
    ctime = ps__filetime_to_unix(ftCreate);
  }

  handle = malloc(sizeof(ps_handle_t));

  if (!handle) {
    ps__no_memory("");
    ps__throw_error();
  }

  handle->pid = cpid;
  handle->create_time = ctime;
  handle->gone = 0;
  handle->wtime = ftCreate;

  PROTECT(res = R_MakeExternalPtr(handle, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(res, psll_finalizer, /* onexit */ 0);
  setAttrib(res, R_ClassSymbol, mkString("ps_handle"));

  UNPROTECT(1);
  return res;
}

SEXP psll_format(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP name, status, result;

  if (!handle) error("Process pointer cleaned up already");

  name = ps__name(handle->pid);
  if (isNull(name)) name = mkString("???");
  PROTECT(name);

  status = ps__status(handle->pid);
  if (isNull(status)) status = mkString("terminated");
  PROTECT(status);

  PROTECT(result = ps__build_list("OldO", name, (long) handle->pid,
				  handle->create_time, status));

  /* We do not check that the pid is still valid here, because we want
     to be able to format & print processes that have finished already. */

  UNPROTECT(3);
  return result;
}

SEXP psll_parent(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP pid, parent, time;
  DWORD ppid;
  FILETIME pft;
  double pctime;
  int ret;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(pid = ps__ppid(handle->pid));
  if (isNull(pid)) ps__throw_error();
  PS__CHECK_HANDLE(handle);

  ppid = INTEGER(pid)[0];

  ret = ps__create_time_raw(ppid, &pft);
  if (ret) ps__throw_error();

  pctime = ps__filetime_to_unix(pft);
  if (pctime > handle->create_time) {
    ps__no_such_process(ppid, 0);
    ps__throw_error();
  }

  PROTECT(time = ScalarReal(pctime));
  PROTECT(parent = psll_handle(pid, time));

  UNPROTECT(3);
  return parent;
}

SEXP psll_ppid(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP ret;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(ret = ps__ppid(handle->pid));
  if (isNull(ret)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return ret;
}


SEXP psll_is_running(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");
  return ps__is_running(handle);
}


SEXP psll_name(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP ret;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(ret = ps__name(handle->pid));
  if (isNull(ret)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return ret;
}

SEXP psll_exe(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");
  result = ps__exe(handle->pid);
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  return result;
}

SEXP psll_cmdline(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  result = ps__get_cmdline(handle->pid);
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  return result;
}

SEXP psll_status(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP ret;

  if (!handle) error("Process pointer cleaned up already");
  ret = ps__status(handle->pid);
  if (isNull(ret)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  return ret;
}

SEXP psll_username(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP ret, result;
  size_t len1, len2;
  char *str;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->pid == 0 || handle->pid == 4) {
    return mkString("NT AUTHORITY\\SYSTEM");
  }

  PROTECT(ret = ps__proc_username(handle->pid));
  if (isNull(ret)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  len1 = strlen(CHAR(STRING_ELT(ret, 0)));
  len2 = strlen(CHAR(STRING_ELT(ret, 1)));
  str = R_alloc(len1 + len2 + 2, sizeof(char));
  memcpy(str, CHAR(STRING_ELT(ret, 0)), len1);
  *(str + len1) = '\\';
  memcpy(str + len1 + 1, CHAR(STRING_ELT(ret,  1)), len2 + 1);

  PROTECT(result = mkString(str));
  UNPROTECT(2);

  return result;
}

SEXP psll_cwd(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->pid == 0 || handle->pid == 4) {
    ps__access_denied("");
    ps__throw_error();
  }

  PROTECT(result = ps__get_cwd(handle->pid));
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return  result;
}

SEXP psll_uids(SEXP p) {
  ps__not_implemented("uids");
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_gids(SEXP p) {
  ps__not_implemented("uids");
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_terminal(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");
  PS__CHECK_HANDLE(handle);
  return ScalarString(NA_STRING);
}

SEXP psll_environ(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->pid == 0 || handle->pid == 4) {
    ps__access_denied("");
    ps__throw_error();
  }

  PROTECT(result = ps__get_environ(handle->pid));
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return  result;
}

SEXP psll_num_threads(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(result = ps__proc_num_threads(handle->pid));
  if (isNull(result)) ps__throw_error();

  PS__CHECK_HANDLE(handle);

  UNPROTECT(1);
  return  result;
}

SEXP psll_cpu_times(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result, result2, names;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(result = ps__proc_cpu_times(handle->pid));
  if (!isNull(result)) {
    PS__CHECK_HANDLE(handle);
    UNPROTECT(1);
    return result;
  }

  UNPROTECT(1);

  PROTECT(result2 = ps__proc_info(handle->pid));
  if (isNull(result2)) {
    UNPROTECT(1);
    ps__throw_error();
  }

  PS__CHECK_HANDLE(handle);

  PROTECT(result = allocVector(REALSXP, 4));
  REAL(result)[0] = REAL(VECTOR_ELT(result2, 2))[0];
  REAL(result)[1] = REAL(VECTOR_ELT(result2, 3))[0];
  REAL(result)[2] = REAL(result)[3] = NA_REAL;
  PROTECT(names = ps__build_string("user", "system", "childen_user",
				   "children_system", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(3);
  return result;
}

SEXP psll_memory_info(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP result, result2, names;

  if (!handle) error("Process pointer cleaned up already");

  PROTECT(result2 = ps__proc_info(handle->pid));
  if (isNull(result2)) {
    UNPROTECT(1);
    ps__throw_error();
  }

  PS__CHECK_HANDLE(handle);

  PROTECT(result = allocVector(REALSXP, 12));
  REAL(result)[0] = REAL(VECTOR_ELT(result2, 12))[0];
  REAL(result)[1] = REAL(VECTOR_ELT(result2, 13))[0];
  REAL(result)[2] = REAL(VECTOR_ELT(result2, 14))[0];
  REAL(result)[3] = REAL(VECTOR_ELT(result2, 15))[0];
  REAL(result)[4] = REAL(VECTOR_ELT(result2, 16))[0];
  REAL(result)[5] = REAL(VECTOR_ELT(result2, 17))[0];
  REAL(result)[6] = REAL(VECTOR_ELT(result2, 18))[0];
  REAL(result)[7] = REAL(VECTOR_ELT(result2, 19))[0];
  REAL(result)[8] = REAL(VECTOR_ELT(result2, 20))[0];
  REAL(result)[9] = REAL(VECTOR_ELT(result2, 21))[0];
  REAL(result)[10] = REAL(VECTOR_ELT(result2, 14))[0];
  REAL(result)[11] = REAL(VECTOR_ELT(result2, 19))[0];
  PROTECT(names = ps__build_string(
    "num_page_faults", "peak_wset", "wset", "peak_paged_pool",
    "paged_pool", "peak_non_paged_pool", "non_paged_pool",
    "pagefile", "peak_pagefile", "mem_private", "rss", "vms", NULL));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(3);
  return result;
}

SEXP psll_send_signal(SEXP p, SEXP sig) {
  ps__not_implemented("uids");
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_suspend(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP running, ret;

  if (!handle) error("Process pointer cleaned up already");

  HANDLE hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  running = ps__is_running(handle);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    goto error;
  }

  PROTECT(ret = ps__proc_suspend(handle->pid));
  if (isNull(ret)) goto error;

  UNPROTECT(1);
  return R_NilValue;

 error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return  R_NilValue;
}

SEXP psll_resume(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP running, ret;

  if (!handle) error("Process pointer cleaned up already");

  HANDLE hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  running = ps__is_running(handle);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    goto error;
  }

  PROTECT(ret = ps__proc_resume(handle->pid));
  if (isNull(ret)) goto error;

  UNPROTECT(1);
  return R_NilValue;

 error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return  R_NilValue;
}

SEXP psll_terminate(SEXP p) {
  ps__not_implemented("uids");
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_kill(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  SEXP running, ret;

  if (!handle) error("Process pointer cleaned up already");

  HANDLE hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  running = ps__is_running(handle);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    goto error;
  }

  PROTECT(ret = ps__proc_kill(handle->pid));
  if (isNull(ret)) ps__throw_error();

  UNPROTECT(1);
  return R_NilValue;

 error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return  R_NilValue;
}

static ULONGLONG (*ps__GetTickCount64)(void) = NULL;

/*
 * Return a double representing the system uptime expressed in seconds
 * since the epoch.
 */
SEXP ps__boot_time() {
  double now, uptime;
  FILETIME fileTime;
  HINSTANCE hKernel32;
  ps__GetTickCount64 = NULL;

  GetSystemTimeAsFileTime(&fileTime);

  now =  ps__filetime_to_unix(fileTime);

  // GetTickCount64() is Windows Vista+ only. Dinamically load
  // GetTickCount64() at runtime. We may have used
  // "#if (_WIN32_WINNT >= 0x0600)" pre-processor but that way
  // the produced exe/wheels cannot be used on Windows XP, see:
  // https://github.com/giampaolo/psutil/issues/811#issuecomment-230639178
  hKernel32 = GetModuleHandleW(L"KERNEL32");
  ps__GetTickCount64 = (void*)GetProcAddress(hKernel32, "GetTickCount64");
  if (ps__GetTickCount64 != NULL) {
    // Windows >= Vista
    uptime = ps__GetTickCount64() / (ULONGLONG)1000.00f;
    return ScalarReal(now - uptime);
  } else {
    // Windows XP.
    // GetTickCount() time will wrap around to zero if the
    // system is run continuously for 49.7 days.
    uptime = GetTickCount() / (LONGLONG)1000.00f;
    return ScalarReal(now - uptime);
  }
}

/*
 * Return the number of logical, active CPUs. Return 0 if undetermined.
 * See discussion at: https://bugs.python.org/issue33166#msg314631
 */
#if (_WIN32_WINNT < 0x0601)  // < Windows 7 (namely Vista and XP)

SEXP ps__cpu_count_logical() {
  return ScalarInteger(NA_INTEGER);
}

SEXP ps__cpu_count_physical() {
  return ScalarInteger(NA_INTEGER);
}

#else  // Windows >= 7

unsigned int ps__get_num_cpus(int fail_on_err) {
  unsigned int ncpus = 0;
  SYSTEM_INFO sysinfo;
  static DWORD(CALLBACK *_GetActiveProcessorCount)(WORD) = NULL;
  HINSTANCE hKernel32;

  // GetActiveProcessorCount is available only on 64 bit versions
  // of Windows from Windows 7 onward.
  // Windows Vista 64 bit and Windows XP doesn't have it.
  hKernel32 = GetModuleHandleW(L"KERNEL32");
  _GetActiveProcessorCount = (void*)GetProcAddress(
    hKernel32, "GetActiveProcessorCount");

  if (_GetActiveProcessorCount != NULL) {
    ncpus = _GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);
    if ((ncpus == 0) && (fail_on_err == 1)) {
      ps__set_error_from_windows_error(0);
    }
  } else {
    ps__debug("GetActiveProcessorCount() not available; "
		 "using GetNativeSystemInfo()");
    GetNativeSystemInfo(&sysinfo);
    ncpus = (unsigned int) sysinfo.dwNumberOfProcessors;
    if ((ncpus == 0) && (fail_on_err == 1)) {
      ps__set_error("GetNativeSystemInfo() failed to retrieve CPU count");
      ps__throw_error();
    }
  }

  return ncpus;
}

SEXP ps__cpu_count_logical() {
  unsigned int ncpus;

  ncpus = ps__get_num_cpus(0);
  if (ncpus != 0) {
    return ScalarInteger(ncpus);
  } else {
    return ScalarInteger(NA_INTEGER);
  }
}

typedef BOOL (WINAPI *LPFN_GLPI)(
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION,
    PDWORD);

// Helper function to count set bits in the processor mask.
DWORD ps__count_set_bits(ULONG_PTR bitMask) {
  DWORD LSHIFT = sizeof(ULONG_PTR)*8 - 1;
  DWORD bitSetCount = 0;
  ULONG_PTR bitTest = (ULONG_PTR)1 << LSHIFT;
  DWORD i;

  for (i = 0; i <= LSHIFT; ++i) {
    bitSetCount += ((bitMask & bitTest)?1:0);
    bitTest/=2;
  }

  return bitSetCount;
}

SEXP ps__cpu_count_physical() {
  LPFN_GLPI glpi;
  BOOL done = FALSE;
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION buffer = NULL;
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION ptr = NULL;
  DWORD returnLength = 0;
  DWORD nproc = 0;
  DWORD byteOffset = 0;

  glpi = (LPFN_GLPI) GetProcAddress(
    GetModuleHandle(TEXT("kernel32")),
    "GetLogicalProcessorInformation");

  if (NULL == glpi) return ScalarInteger(NA_INTEGER);

  while (!done) {
    DWORD rc = glpi(buffer, &returnLength);

    if (FALSE == rc) {
      if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	if (buffer) free(buffer);
	buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION)  malloc(returnLength);

	if (NULL == buffer) {
	  ps__no_memory("");
	  ps__throw_error();
	}
      } else {
	ps__set_error_from_windows_error(0);
	ps__throw_error();
      }
    } else {
      done = TRUE;
    }
  }

  ptr = buffer;

  while (byteOffset + sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION) <= returnLength) {

    switch (ptr->Relationship) {
    case RelationProcessorCore:
      // A hyperthreaded core supplies more than one logical processor.
      nproc += ps__count_set_bits(ptr->ProcessorMask);
      break;
    default:
      break;
    }

    byteOffset += sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
    ptr++;
  }

  free(buffer);

  if (nproc > 0) {
    return ScalarInteger(nproc);
  } else {
    return ScalarInteger(NA_INTEGER);
  }
}
#endif

SEXP ps__kill_if_env(SEXP marker, SEXP after, SEXP pid, SEXP sig) {
  const char *cmarker = CHAR(STRING_ELT(marker, 0));
  double cafter = REAL(after)[0];
  DWORD cpid = INTEGER(pid)[0];
  SEXP env;
  size_t i, len;
  double ctime = 0, ctime2 = 0;

  /* Filter on start time */
  FILETIME ftCreate;
  int ret = ps__create_time_raw(cpid, &ftCreate);
  if (ret) ps__throw_error();
  ctime = ps__filetime_to_unix(ftCreate);
  if (ctime < cafter - 1) return R_NilValue;

  PROTECT(env = ps__get_environ(cpid));
  if (isNull(env)) ps__throw_error();

  len = LENGTH(env);

  for (i = 0; i < len; i++) {
    if (strstr(CHAR(STRING_ELT(env, i)), cmarker)) {
      HANDLE hProcess = ps__handle_from_pid(cpid);
      FILETIME ftCreate;
      SEXP name, ret2;
      int ret = ps__create_time_raw(cpid, &ftCreate);
      if (ret) {
	CloseHandle(hProcess);
	ps__throw_error();
      }

      ctime2 = ps__filetime_to_unix(ftCreate);
      if (fabs(ctime - ctime2) < 0.01)  {
	PROTECT(name = ps__name(cpid));
	ret2 = ps__proc_kill(cpid);
	CloseHandle(hProcess);
	if (isNull(ret2)) ps__throw_error();
	if (isNull(name)) {
	  UNPROTECT(2);
	  return mkString("???");
	} else {
	  UNPROTECT(2);
	  return name;
	}
      } else  {
	CloseHandle(hProcess);
	UNPROTECT(1);
	return R_NilValue;
      }
    }
  }

  UNPROTECT(1);
  return R_NilValue;
}

SEXP ps__find_if_env(SEXP marker, SEXP after, SEXP pid) {
  const char *cmarker = CHAR(STRING_ELT(marker, 0));
  double cafter = REAL(after)[0];
  long cpid = INTEGER(pid)[0];
  SEXP env;
  size_t i, len;
  SEXP phandle;
  ps_handle_t *handle;
  double ctime = 0;
  FILETIME ftCreate;

  /* Filter on start time */
  int ret = ps__create_time_raw(cpid, &ftCreate);
  if (ret) ps__throw_error();
  ctime = ps__filetime_to_unix(ftCreate);
  if (ctime < cafter - 1) return R_NilValue;

  PROTECT(phandle = psll_handle(pid, R_NilValue));
  handle = R_ExternalPtrAddr(phandle);

  PROTECT(env = ps__get_environ(cpid));
  if (isNull(env)) ps__throw_error();

  len = LENGTH(env);

  for (i = 0; i < len; i++) {
    if (strstr(CHAR(STRING_ELT(env, i)), cmarker)) {
      UNPROTECT(2);
      PS__CHECK_HANDLE(handle);
      return phandle;
    }
  }

  UNPROTECT(2);
  return R_NilValue;
}

SEXP psll_num_fds(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE  hProcess = NULL;
  DWORD handleCount;

  if (!handle) error("Process pointer cleaned up already");

  hProcess = ps__handle_from_pid(handle->pid);
  if (hProcess != NULL) {
    if (GetProcessHandleCount(hProcess, &handleCount)) {
      CloseHandle(hProcess);
      PS__CHECK_HANDLE(handle);
      return ScalarInteger(handleCount);
    }
  }

  /* Cleanup on error */
  if (hProcess != NULL) CloseHandle(hProcess);
  PS__CHECK_HANDLE(handle);
  ps__set_error_from_windows_error(0);
  ps__throw_error();
  return R_NilValue;
}

SEXP psll_open_files(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE processHandle = NULL;
  DWORD access = PROCESS_DUP_HANDLE | PROCESS_QUERY_INFORMATION;
  SEXP result;

  if (!handle) error("Process pointer cleaned up already");

  processHandle = ps__handle_from_pid_waccess(handle->pid, access);
  if (processHandle == NULL) {
    PS__CHECK_HANDLE(handle);
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  PROTECT(result = ps__get_open_files(handle->pid, processHandle));

  CloseHandle(processHandle);

  PS__CHECK_HANDLE(handle);

  if (isNull(result)) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  UNPROTECT(1);
  return result;
}

SEXP psll_interrupt(SEXP p, SEXP ctrlc, SEXP interrupt_path) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  const char *cinterrupt_path = CHAR(STRING_ELT(interrupt_path, 0));
  int cctrlc = LOGICAL(ctrlc)[0];
  WCHAR *wpath;
  SEXP running;
  int iret;
  STARTUPINFOW startup = { 0 };
  PROCESS_INFORMATION info = { 0 };
  HANDLE hProcess;
  char arguments[100];
  WCHAR *warguments;
  DWORD process_flags;

  if (!handle) error("Process pointer cleaned up already");

  hProcess = ps__handle_from_pid(handle->pid);
  if (!hProcess) goto error;

  running = ps__is_running(handle);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    goto error;
  }

  iret = ps__utf8_to_utf16(cinterrupt_path, &wpath);
  if (iret) goto error;

  iret = snprintf(arguments, sizeof(arguments) - 1, "interrupt.exe %d %s", handle->pid,
		  cctrlc ? "c" : "break");
  if  (iret < 0) goto error;

  iret = ps__utf8_to_utf16(arguments, &warguments);
  if (iret) goto error;

  startup.cb = sizeof(startup);
  startup.lpReserved = NULL;
  startup.lpDesktop = NULL;
  startup.lpTitle = NULL;
  startup.dwFlags = 0;

  startup.cbReserved2 = 0;
  startup.lpReserved2 = 0;

  process_flags = CREATE_UNICODE_ENVIRONMENT | CREATE_NO_WINDOW;

  iret = CreateProcessW(
    /* lpApplicationName =    */ wpath,
    /* lpCommandLine =        */ warguments,
    /* lpProcessAttributes =  */ NULL,
    /* lpThreadAttributes =   */ NULL,
    /* bInheritHandles =      */ 0,
    /* dwCreationFlags =      */ process_flags,
    /* lpEnvironment =        */ NULL,
    /* lpCurrentDirectory =   */ NULL,
    /* lpStartupInfo =        */ &startup,
    /* lpProcessInformation = */ &info);

  if (!iret) {
    ps__set_error_from_errno(0);
    goto error;
  }

  CloseHandle(info.hThread);
  CloseHandle(info.hProcess);

  return R_NilValue;

 error:
  if (hProcess) CloseHandle(hProcess);
  ps__throw_error();
  return R_NilValue;
}

SEXP ps__users() {
  HANDLE hServer = WTS_CURRENT_SERVER_HANDLE;
  WCHAR *buffer_user = NULL;
  LPTSTR buffer_addr = NULL;
  PWTS_SESSION_INFO sessions = NULL;
  DWORD count;
  DWORD i;
  DWORD sessionId;
  DWORD bytes;
  PWTS_CLIENT_ADDRESS address;
  char address_str[50];
  double unix_time;

  PWINSTATIONQUERYINFORMATIONW WinStationQueryInformationW;
  WINSTATION_INFO station_info;
  HINSTANCE hInstWinSta = NULL;
  ULONG returnLen;

  SEXP retlist, raddress, username;

  hInstWinSta = LoadLibraryA("winsta.dll");
  WinStationQueryInformationW = (PWINSTATIONQUERYINFORMATIONW) \
    GetProcAddress(hInstWinSta, "WinStationQueryInformationW");

  if (WTSEnumerateSessions(hServer, 0, 1, &sessions, &count) == 0) {
    ps__set_error_from_windows_error(0);
    goto error;
  }

  PROTECT(retlist = allocVector(VECSXP, count));

  for (i = 0; i < count; i++) {
    SET_VECTOR_ELT(retlist, i, R_NilValue);
    raddress = R_NilValue;
    sessionId = sessions[i].SessionId;
    if (buffer_user != NULL)
      WTSFreeMemory(buffer_user);
    if (buffer_addr != NULL)
      WTSFreeMemory(buffer_addr);

    buffer_user = NULL;
    buffer_addr = NULL;

    // username
    bytes = 0;
    if (WTSQuerySessionInformationW(hServer, sessionId, WTSUserName,
				    &buffer_user, &bytes) == 0) {
      ps__set_error_from_windows_error(0);
      goto error;
    }
    if (bytes <= 2) continue;

    // address
    bytes = 0;
    if (WTSQuerySessionInformation(hServer, sessionId, WTSClientAddress,
				   &buffer_addr, &bytes) == 0) {
      ps__set_error_from_windows_error(0);
      goto error;
    }

    address = (PWTS_CLIENT_ADDRESS) buffer_addr;
    if (address->AddressFamily == 0 &&  // AF_INET
	(address->Address[0] || address->Address[1] ||
	 address->Address[2] || address->Address[3])) {
      snprintf(address_str,
	       sizeof(address_str),
	       "%u.%u.%u.%u",
	       address->Address[0],
	       address->Address[1],
	       address->Address[2],
	       address->Address[3]);
      raddress = mkString(address_str);
    } else {
      raddress = mkString("");
    }
    PROTECT(raddress);

    // login time
    if (!WinStationQueryInformationW(hServer,
				     sessionId,
				     WinStationInformation,
				     &station_info,
				     sizeof(station_info),
				     &returnLen))  {
	goto error;
      }

    unix_time = ps__filetime_to_unix(station_info.ConnectTime);

    PROTECT(username = ps__utf16_to_strsxp(buffer_user, -1));

    SET_VECTOR_ELT(
      retlist, i,
      ps__build_list("OOOdi", username, ScalarString(NA_STRING), raddress,
		     unix_time, NA_INTEGER));
    UNPROTECT(2);
  }

  WTSFreeMemory(sessions);
  WTSFreeMemory(buffer_user);
  WTSFreeMemory(buffer_addr);
  FreeLibrary(hInstWinSta);

  UNPROTECT(1);
  return retlist;

 error:
  if (hInstWinSta != NULL) FreeLibrary(hInstWinSta);
  if (sessions != NULL) WTSFreeMemory(sessions);
  if (buffer_user != NULL) WTSFreeMemory(buffer_user);
  if (buffer_addr != NULL) WTSFreeMemory(buffer_addr);
  ps__throw_error();
  return R_NilValue;
}
