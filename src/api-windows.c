
#include "common.h"
#include "windows.h"
#include "arch/windows/process_info.h"
#include "arch/windows/process_handles.h"

#include <tlhelp32.h>
#include <string.h>
#include <math.h>

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

static void PS__CHECK_HANDLE(ps_handle_t *handle) {
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
  if (isNull(ret)) ps__throw_error();

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
  if (isNull(ret)) ps__throw_error();

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
