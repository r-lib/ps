
#include "common.h"
#include "arch/windows/security.h"
#include "arch/windows/process_info.h"
#include "arch/windows/process_handles.h"

void psll_finalizer(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (handle) free(handle);
}

void ps__wrap_windows_error(ps_handle_t *handle) {
  /* TODO */
}

int psll__proc_is_suspended(DWORD pid) {
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

SEXP psll_handle(SEXP pid, SEXP time) {
  DWORD cpid = isNull(pid) ? GetCurrentProcessId() : INTEGER(pid)[0];
  double ctime;
  long long unix_time;
  ps_handle_t *handle;
  SEXP res;
  FILETIME ftCreate = { 0, 0 };

  if (!isNull(time)) {
    ctime = REAL(time)[0];

  } else {
    HANDLE hProcess = ps__handle_from_pid(cpid);
    FILETIME ftExit, ftKernel, ftUser;
    if (! hProcess ||
	! GetProcessTimes(hProcess, &ftCreate, &ftExit, &ftKernel, &ftUser)) {
      if (hProcess)  CloseHandle(hProcess);
      if (GetLastError() == ERROR_ACCESS_DENIED) {
	// usually means the process has died so we throw a
	// NoSuchProcess here
	ps__no_such_process(cpid, 0);
      } else {
	psw__set_error_from_windows_error(0);
      }
      ps__throw_error();

    }

    CloseHandle(hProcess);

    // Convert the FILETIME structure to a Unix time.
    // It's the best I could find by googling and borrowing code here
    // and there. The time returned has a precision of 1 second.
    unix_time = ((LONGLONG)ftCreate.dwHighDateTime) << 32;
    unix_time += ftCreate.dwLowDateTime - 116444736000000000LL;
    unix_time /= 10000000;
    ctime = (double)unix_time;
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
  /* TODO */
  return  R_NilValue;
}

SEXP psll_parent(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_ppid(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_is_running(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_name(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);

  if (!handle) error("Process pointer cleaned up already");

  /* This is how PIDs 0 and 4 are always represented in taskmgr */
  /* and process-hacker. */
  if (handle->pid == 0) return mkString("System Idle Process");
  if (handle->pid == 4) return mkString("System");

  /* TODO */

  return  R_NilValue;
}

SEXP psll_exe(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  HANDLE hProcess;
  wchar_t exe[MAX_PATH];

  if (!handle) error("Process pointer cleaned up already");
  if (handle->pid == 0 || handle->pid == 4) return ScalarString(NA_STRING);

  hProcess = ps__handle_from_pid_waccess(handle->pid, PROCESS_QUERY_INFORMATION);
  if (NULL == hProcess){
    psw__set_error_from_windows_error(0);
    ps__throw_error();
  }

  if (GetProcessImageFileNameW(hProcess, exe, MAX_PATH) == 0) {
    CloseHandle(hProcess);
    psw__set_error_from_windows_error(0);
    ps__throw_error();
  }
  CloseHandle(hProcess);

  /* TODO: convert path */
  return ScalarString(psw__utf16_to_charsxp(exe, -1));
}

SEXP psll_cmdline(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_status(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  int ret;

  if (!handle) error("Process pointer cleaned up already");

  ret = psll__proc_is_suspended(handle->pid);
  if (ret < 0) {
    ps__wrap_windows_error(handle);
    ps__throw_error();
  }

  return ret ? mkString("stopped") : mkString("running");
}

SEXP psll_username(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_cwd(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_uids(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_gids(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_terminal(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_environ(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_num_threads(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_cpu_times(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_memory_info(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_send_signal(SEXP p, SEXP sig) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_suspend(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_resume(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_terminate(SEXP p) {
  /* TODO */
  return  R_NilValue;
}

SEXP psll_kill(SEXP p) {
  /* TODO */
  return  R_NilValue;
}
