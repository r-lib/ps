/*
 * Copyright (c) 2009, Jay Loden, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *
 * Windows platform-specific module methods for _psutil_windows
 */

// Fixes clash between winsock2.h and windows.h
#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <Psapi.h>
#include <time.h>
#include <lm.h>
#include <WinIoCtl.h>
#include <tchar.h>
#include <tlhelp32.h>
#include <winsock2.h>
#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
#include <ws2tcpip.h>
#endif
#if defined(__MINGW32__)
#include <Wincrypt.h>
#endif
#include <iphlpapi.h>
#include <wtsapi32.h>
#include <Winsvc.h>
#include <PowrProf.h>
#include <signal.h>

#include "common.h"
#include "arch/windows/security.h"
#include "arch/windows/process_info.h"
#include "arch/windows/process_handles.h"
#ifndef __MINGW32__
#include "arch/windows/ntextapi.h"
#include "arch/windows/inet_ntop.h"
#else
#include <winternl.h>
#include <ws2tcpip.h>
#endif
#include "arch/windows/services.h"

#include <stdlib.h>

/*
 * ============================================================================
 * Utilities
 * ============================================================================
 */

#define MALLOC(x) HeapAlloc(GetProcessHeap(), 0, (x))
#define FREE(x) HeapFree(GetProcessHeap(), 0, (x))
#define LO_T 1e-7
#define HI_T 429.4967296
#define BYTESWAP_USHORT(x) ((((USHORT)(x) << 8) | ((USHORT)(x) >> 8)) & 0xffff)
#ifndef AF_INET6
#define AF_INET6 23
#endif

#if (_WIN32_WINNT >= 0x0601)  // Windows  7
typedef BOOL (WINAPI *PFN_GETLOGICALPROCESSORINFORMATIONEX)(
LOGICAL_PROCESSOR_RELATIONSHIP relationship,
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX Buffer,
  PDWORD ReturnLength);
static PFN_GETLOGICALPROCESSORINFORMATIONEX _GetLogicalProcessorInformationEx;
#endif

// Fix for mingw32, see:
// https://github.com/giampaolo/psutil/issues/351#c2
// This is actually a DISK_PERFORMANCE struct:
// https://msdn.microsoft.com/en-us/library/windows/desktop/
//     aa363991(v=vs.85).aspx
typedef struct _DISK_PERFORMANCE_WIN_2008 {
  LARGE_INTEGER BytesRead;
  LARGE_INTEGER BytesWritten;
  LARGE_INTEGER ReadTime;
  LARGE_INTEGER WriteTime;
  LARGE_INTEGER IdleTime;
  DWORD         ReadCount;
  DWORD         WriteCount;
  DWORD         QueueDepth;
  DWORD         SplitCount;
  LARGE_INTEGER QueryTime;
  DWORD         StorageDeviceNumber;
  WCHAR         StorageManagerName[8];
} DISK_PERFORMANCE_WIN_2008;

// --- network connections mingw32 support
#ifndef _IPRTRMIB_H
#if (_WIN32_WINNT < 0x0600) // Windows XP
typedef struct _MIB_TCP6ROW_OWNER_PID {
UCHAR ucLocalAddr[16];
DWORD dwLocalScopeId;
DWORD dwLocalPort;
UCHAR ucRemoteAddr[16];
DWORD dwRemoteScopeId;
DWORD dwRemotePort;
DWORD dwState;
DWORD dwOwningPid;
} MIB_TCP6ROW_OWNER_PID, *PMIB_TCP6ROW_OWNER_PID;

typedef struct _MIB_TCP6TABLE_OWNER_PID {
  DWORD dwNumEntries;
  MIB_TCP6ROW_OWNER_PID table[ANY_SIZE];
} MIB_TCP6TABLE_OWNER_PID, *PMIB_TCP6TABLE_OWNER_PID;
#endif
#endif

#ifndef __IPHLPAPI_H__
typedef struct in6_addr {
  union {
    UCHAR Byte[16];
    USHORT Word[8];
  } u;
} IN6_ADDR, *PIN6_ADDR, FAR *LPIN6_ADDR;

typedef enum _UDP_TABLE_CLASS {
  UDP_TABLE_BASIC,
  UDP_TABLE_OWNER_PID,
  UDP_TABLE_OWNER_MODULE
} UDP_TABLE_CLASS, *PUDP_TABLE_CLASS;

typedef struct _MIB_UDPROW_OWNER_PID {
  DWORD dwLocalAddr;
  DWORD dwLocalPort;
  DWORD dwOwningPid;
} MIB_UDPROW_OWNER_PID, *PMIB_UDPROW_OWNER_PID;

typedef struct _MIB_UDPTABLE_OWNER_PID {
  DWORD dwNumEntries;
  MIB_UDPROW_OWNER_PID table[ANY_SIZE];
} MIB_UDPTABLE_OWNER_PID, *PMIB_UDPTABLE_OWNER_PID;
#endif

#if (_WIN32_WINNT < 0x0600) // Windows XP
#if (!defined(__MINGW32__))
typedef struct _MIB_UDP6ROW_OWNER_PID {
UCHAR ucLocalAddr[16];
DWORD dwLocalScopeId;
DWORD dwLocalPort;
DWORD dwOwningPid;
} MIB_UDP6ROW_OWNER_PID, *PMIB_UDP6ROW_OWNER_PID;

typedef struct _MIB_UDP6TABLE_OWNER_PID {
  DWORD dwNumEntries;
  MIB_UDP6ROW_OWNER_PID table[ANY_SIZE];
} MIB_UDP6TABLE_OWNER_PID, *PMIB_UDP6TABLE_OWNER_PID;
#endif
#endif

typedef struct _PROCESSOR_POWER_INFORMATION {
  ULONG Number;
  ULONG MaxMhz;
  ULONG CurrentMhz;
  ULONG MhzLimit;
  ULONG MaxIdleState;
  ULONG CurrentIdleState;
} PROCESSOR_POWER_INFORMATION, *PPROCESSOR_POWER_INFORMATION;

#if (0)

PIP_ADAPTER_ADDRESSES
ps__get_nic_addresses() {
  // allocate a 15 KB buffer to start with
  ULONG outBufLen = 15000;
  DWORD dwRetVal = 0;
  ULONG attempts = 0;
  PIP_ADAPTER_ADDRESSES pAddresses = NULL;

  do {
    pAddresses = (IP_ADAPTER_ADDRESSES *) malloc(outBufLen);
    if (pAddresses == NULL) {
      ps__no_memory("");
      ps__throw_error();
    }

    dwRetVal = GetAdaptersAddresses(AF_UNSPEC, 0, NULL, pAddresses,
				    &outBufLen);
    if (dwRetVal == ERROR_BUFFER_OVERFLOW) {
      free(pAddresses);
      pAddresses = NULL;
    }
    else {
      break;
    }

    attempts++;
  } while ((dwRetVal == ERROR_BUFFER_OVERFLOW) && (attempts < 3));

  if (dwRetVal != NO_ERROR) {
    ps__set_error("GetAdaptersAddresses() syscall failed.");
    ps__throw_error();
  }

  return pAddresses;
}


/*
 * Return the number of logical, active CPUs. Return 0 if undetermined.
 * See discussion at: https://bugs.python.org/issue33166#msg314631
 */
unsigned int
ps__get_num_cpus(int fail_on_err) {
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
  }
  else {
    ps__debug("GetActiveProcessorCount() not available; "
	      "using GetNativeSystemInfo()");
    GetNativeSystemInfo(&sysinfo);
    ncpus = (unsigned int)sysinfo.dwNumberOfProcessors;
    if ((ncpus == 0) && (fail_on_err == 1)) {
      PyErr_SetString(
		      PyExc_RuntimeError,
		      "GetNativeSystemInfo() failed to retrieve CPU count");
    }
  }
  return ncpus;
}

/*
 * ============================================================================
 * Public Python API
 * ============================================================================
 */

#endif

static ULONGLONG (*ps__GetTickCount64)(void) = NULL;

/*
 * Return a Python float representing the system uptime expressed in seconds
 * since the epoch.
 */
SEXP ps__boot_time() {
#if (_WIN32_WINNT >= 0x0600)  // Windows Vista
  ULONGLONG uptime;
#else
  double uptime;
#endif
  time_t pt;
  FILETIME fileTime;
  long long ll;
  HINSTANCE hKernel32;
  ps__GetTickCount64 = NULL;

  GetSystemTimeAsFileTime(&fileTime);

  /*
    HUGE thanks to:
    http://johnstewien.spaces.live.com/blog/cns!E6885DB5CEBABBC8!831.entry

    This function converts the FILETIME structure to the 32 bit
    Unix time structure.
    The time_t is a 32-bit value for the number of seconds since
    January 1, 1970. A FILETIME is a 64-bit for the number of
    100-nanosecond periods since January 1, 1601. Convert by
    subtracting the number of 100-nanosecond period betwee 01-01-1970
    and 01-01-1601, from time_t the divide by 1e+7 to get to the same
    base granularity.
  */
  ll = ((
#if (_WIN32_WINNT >= 0x0600)  // Windows Vista
	 (ULONGLONG)
#else
	 (LONGLONG)
#endif
	 (fileTime.dwHighDateTime)) << 32) + fileTime.dwLowDateTime;
  pt = (time_t)((ll - 116444736000000000ull) / 10000000ull);

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
    return ScalarReal(pt - uptime);
  }
  else {
    // Windows XP.
    // GetTickCount() time will wrap around to zero if the
    // system is run continuously for 49.7 days.
    uptime = GetTickCount() / (LONGLONG)1000.00f;
    return ScalarReal(pt - uptime);
  }
}


/*
 * Return 1 if PID exists in the current process list, else 0.
 */
SEXP ps__pid_exists(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  int status;

  status = ps__pid_is_running(pid);
  if (-1 == status)
    ps__throw_error(); // exception raised in ps__pid_is_running()
  return ScalarLogical(status);
}


/*
 * Return a Python list of all the PIDs running on the system.
 */
SEXP ps__pids() {
  DWORD *proclist = NULL;
  DWORD numberOfReturnedPIDs;
  DWORD i;
  SEXP retlist;

  proclist = ps__get_pids(&numberOfReturnedPIDs);
  if (proclist == NULL)
    ps__throw_error();

  PROTECT(retlist = allocVector(INTSXP, numberOfReturnedPIDs));

  for (i = 0; i < numberOfReturnedPIDs; i++) {
    INTEGER(retlist)[i] = proclist[i];
  }

  // free C array allocated for PIDs
  free(proclist);
  UNPROTECT(1);
  return retlist;
}

/*
 * Kill a process given its PID.
 */
SEXP ps__proc_kill(SEXP r_pid) {
  HANDLE hProcess;
  DWORD err;
  long pid = INTEGER(r_pid)[0];

  if (pid == 0) {
    ps__access_denied("");
    ps__throw_error();
  }

  hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, pid);
  if (hProcess == NULL) {
    if (GetLastError() == ERROR_INVALID_PARAMETER) {
      // see https://github.com/giampaolo/psutil/issues/24
      ps__debug("OpenProcess -> ERROR_INVALID_PARAMETER turned "
		   "into NoSuchProcess");
      ps__no_such_process("");
    }
    else {
      ps__set_error_from_windows_error(0);
    }
    return NULL;
  }

  // kill the process
  if (! TerminateProcess(hProcess, SIGTERM)) {
    err = GetLastError();
    // See: https://github.com/giampaolo/psutil/issues/1099
    if (err != ERROR_ACCESS_DENIED) {
      CloseHandle(hProcess);
      ps__set_error_from_windows_error(0);
      return NULL;
    }
  }

  CloseHandle(hProcess);
  return R_NilValue;
}

#if (0)

/*
 * Wait for process to terminate and return its exit code.
 */
static PyObject *
ps__proc_wait(PyObject *self, PyObject *args) {
  HANDLE hProcess;
  DWORD ExitCode;
  DWORD retVal;
  long pid;
  long timeout;

  if (! PyArg_ParseTuple(args, "ll", &pid, &timeout))
    return NULL;
  if (pid == 0)
    return AccessDenied("");

  hProcess = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_INFORMATION,
			 FALSE, pid);
  if (hProcess == NULL) {
    if (GetLastError() == ERROR_INVALID_PARAMETER) {
      // no such process; we do not want to raise NSP but
      // return None instead.
      Py_RETURN_NONE;
    }
    else
      return PyErr_SetFromWindowsErr(0);
  }

  // wait until the process has terminated
  Py_BEGIN_ALLOW_THREADS
    retVal = WaitForSingleObject(hProcess, timeout);
  Py_END_ALLOW_THREADS

    // handle return code
    if (retVal == WAIT_FAILED) {
      CloseHandle(hProcess);
      PyErr_SetFromWindowsErr(0);
      return NULL;
    }
  if (retVal == WAIT_TIMEOUT) {
    CloseHandle(hProcess);
    PyErr_SetString(TimeoutExpired,
		    "WaitForSingleObject() returned WAIT_TIMEOUT");
    return NULL;
  }
  if (retVal == WAIT_ABANDONED) {
    ps__debug("WaitForSingleObject() -> WAIT_ABANDONED");
    CloseHandle(hProcess);
    PyErr_SetString(TimeoutAbandoned,
		    "WaitForSingleObject() returned WAIT_ABANDONED");
    return NULL;
  }

  // WaitForSingleObject() returned WAIT_OBJECT_0. It means the
  // process is gone so we can get its process exit code. The PID
  // may still stick around though but we'll handle that from Python.
  if (GetExitCodeProcess(hProcess, &ExitCode) == 0) {
    CloseHandle(hProcess);
    return PyErr_SetFromWindowsErr(GetLastError());
  }
  CloseHandle(hProcess);

#if PY_MAJOR_VERSION >= 3
  return PyLong_FromLong((long) ExitCode);
#else
  return PyInt_FromLong((long) ExitCode);
#endif
}

#endif

/*
 * Return a Python tuple (user_time, kernel_time)
 */
SEXP ps__proc_cpu_times(SEXP r_pid) {
  long        pid = INTEGER(r_pid)[0];
  HANDLE      hProcess;
  FILETIME    ftCreate, ftExit, ftKernel, ftUser;

  hProcess = ps__handle_from_pid(pid);
  if (hProcess == NULL)
    ps__throw_error();
  if (! GetProcessTimes(hProcess, &ftCreate, &ftExit, &ftKernel, &ftUser)) {
    CloseHandle(hProcess);
    if (GetLastError() == ERROR_ACCESS_DENIED) {
      // usually means the process has died so we throw a NoSuchProcess
      // here
      ps__no_such_process("");
      ps__throw_error();
    }
    else {
      ps__set_error_from_windows_error(0);
      ps__throw_error();
    }
  }

  CloseHandle(hProcess);

  /*
   * User and kernel times are represented as a FILETIME structure
   * wich contains a 64-bit value representing the number of
   * 100-nanosecond intervals since January 1, 1601 (UTC):
   * http://msdn.microsoft.com/en-us/library/ms724284(VS.85).aspx
   * To convert it into a float representing the seconds that the
   * process has executed in user/kernel mode I borrowed the code
   * below from Python's Modules/posixmodule.c
   */
  return ps__build_list(
    "dd",
    (double)(ftUser.dwHighDateTime * 429.4967296 + ftUser.dwLowDateTime * 1e-7),
    (double)(ftKernel.dwHighDateTime * 429.4967296 + ftKernel.dwLowDateTime * 1e-7));
}


/*
 * Return a Python float indicating the process create time expressed in
 * seconds since the epoch.
 */
SEXP ps__proc_create_time(SEXP r_pid) {
  long        pid = INTEGER(r_pid)[0];
  long long   unix_time;
  HANDLE      hProcess;
  FILETIME    ftCreate, ftExit, ftKernel, ftUser;

  // special case for PIDs 0 and 4, return system boot time
  if (0 == pid || 4 == pid)
    return ps__boot_time();

  hProcess = ps__handle_from_pid(pid);
  if (hProcess == NULL)
    ps__throw_error();
  if (! GetProcessTimes(hProcess, &ftCreate, &ftExit, &ftKernel, &ftUser)) {
    CloseHandle(hProcess);
    if (GetLastError() == ERROR_ACCESS_DENIED) {
      // usually means the process has died so we throw a
      // NoSuchProcess here
      ps__no_such_process("");
      ps__throw_error();
    }
    else {
      ps__set_error_from_windows_error(0);
      ps__throw_error();
    }
  }

  CloseHandle(hProcess);

  /*
  // Make sure the process is not gone as OpenProcess alone seems to be
  // unreliable in doing so (it seems a previous call to p.wait() makes
  // it unreliable).
  // This check is important as creation time is used to make sure the
  // process is still running.
  ret = GetExitCodeProcess(hProcess, &exitCode);
  CloseHandle(hProcess);
  if (ret != 0) {
  if (exitCode != STILL_ACTIVE)
  return NoSuchProcess("");
  }
  else {
  // Ignore access denied as it means the process is still alive.
  // For all other errors, we want an exception.
  if (GetLastError() != ERROR_ACCESS_DENIED)
  return PyErr_SetFromWindowsErr(0);
  }
  */

  // Convert the FILETIME structure to a Unix time.
  // It's the best I could find by googling and borrowing code here
  // and there. The time returned has a precision of 1 second.
  unix_time = ((LONGLONG)ftCreate.dwHighDateTime) << 32;
  unix_time += ftCreate.dwLowDateTime - 116444736000000000LL;
  unix_time /= 10000000;
  return ScalarReal((double)unix_time);
}

#if (0)

/*
 * Return the number of active, logical CPUs.
 */
static PyObject *
ps__cpu_count_logical(PyObject *self, PyObject *args) {
  unsigned int ncpus;

  ncpus = ps__get_num_cpus(0);
  if (ncpus != 0)
    return Py_BuildValue("I", ncpus);
  else
    Py_RETURN_NONE;  // mimick os.cpu_count()
}


/*
 * Return the number of physical CPU cores (hyper-thread CPUs count
 * is excluded).
 */
#if (_WIN32_WINNT < 0x0601)  // < Windows 7 (namely Vista and XP)
static PyObject *
ps__cpu_count_phys(PyObject *self, PyObject *args) {
  // Note: we may have used GetLogicalProcessorInformation()
  // but I don't want to prolong support for Windows XP and Vista.
  // On such old systems psutil will compile but this API will
  // just return None.
  ps__debug("Win < 7; cpu_count_phys() forced to None");
  Py_RETURN_NONE;
}
#else  // Windows >= 7
static PyObject *
ps__cpu_count_phys(PyObject *self, PyObject *args) {
  DWORD rc;
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX buffer = NULL;
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX ptr = NULL;
  DWORD length = 0;
  DWORD offset = 0;
  DWORD ncpus = 0;

  // GetLogicalProcessorInformationEx() is available from Windows 7
  // onward. Differently from GetLogicalProcessorInformation()
  // it supports process groups, meaning this is able to report more
  // than 64 CPUs. See:
  // https://bugs.python.org/issue33166
  _GetLogicalProcessorInformationEx = \
    (PFN_GETLOGICALPROCESSORINFORMATIONEX)GetProcAddress(
							 GetModuleHandle(TEXT("kernel32")),
							 "GetLogicalProcessorInformationEx");
  if (_GetLogicalProcessorInformationEx == NULL) {
    ps__debug("failed loading GetLogicalProcessorInformationEx()");
    goto return_none;
  }

  while (1) {
    rc = _GetLogicalProcessorInformationEx(
					   RelationAll, buffer, &length);
    if (rc == FALSE) {
      if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	if (buffer) {
	  free(buffer);
	}
	buffer = \
	  (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX)malloc(length);
	if (NULL == buffer) {
	  PyErr_NoMemory();
	  return NULL;
	}
      }
      else {
	ps__debug("GetLogicalProcessorInformationEx() returned ",
		     GetLastError());
	goto return_none;
      }
    }
    else {
      break;
    }
  }

  ptr = buffer;
  while (ptr->Size > 0 && offset + ptr->Size <= length) {
    if (ptr->Relationship == RelationProcessorCore) {
      ncpus += 1;
    }
    offset += ptr->Size;
    ptr = (SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX*)\
      (((char*)ptr) + ptr->Size);
  }

  free(buffer);
  if (ncpus != 0) {
    return Py_BuildValue("I", ncpus);
  }
  else {
    ps__debug("GetLogicalProcessorInformationEx() count was 0");
    Py_RETURN_NONE;  // mimick os.cpu_count()
  }

 return_none:
  if (buffer != NULL)
    free(buffer);
  Py_RETURN_NONE;
}
#endif

#endif


/*
 * Return process cmdline as a Python list of cmdline arguments.
 */
SEXP ps__proc_cmdline(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  int pid_return;

  if ((pid == 0) || (pid == 4))
    return allocVector(STRSXP, 0);

  pid_return = ps__pid_is_running(pid);
  if (pid_return == 0) {
    ps__no_such_process("");
    ps__throw_error();
  }

  if (pid_return == -1)
    ps__throw_error();

  return ps__get_cmdline(pid);
}


/*
 * Return process cmdline as a Python list of cmdline arguments.
 */
SEXP ps__proc_environ(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  int pid_return;

  if ((pid == 0) || (pid == 4))
    return allocVector(STRSXP, 0);

  pid_return = ps__pid_is_running(pid);
  if (pid_return == 0) {
    ps__no_such_process("");
    ps__throw_error();
  }
  if (pid_return == -1)
    ps__throw_error();

  return ps__get_environ(pid);
}


/*
 * Return process executable path.
 */
SEXP ps__proc_exe(SEXP r_pid) {
  long pid =  INTEGER(r_pid)[0];
  HANDLE hProcess;
  wchar_t exe[MAX_PATH];

  hProcess = ps__handle_from_pid_waccess(pid, PROCESS_QUERY_INFORMATION);
  if (NULL == hProcess)
    error("No  such process");

  if (GetProcessImageFileNameW(hProcess, exe, MAX_PATH) == 0) {
    CloseHandle(hProcess);
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }
  CloseHandle(hProcess);
  return ScalarString(ps__utf16_to_charsxp(exe, -1));
}

/*
 * Return process base name.
 * Note: ps__proc_exe() is attempted first because it's faster
 * but it raise AccessDenied for processes owned by other users
 * in which case we fall back on using this.
 */
SEXP ps__proc_name(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];;
  int ok;
  PROCESSENTRY32W pentry;
  HANDLE hSnapShot;

  hSnapShot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, pid);
  if (hSnapShot == INVALID_HANDLE_VALUE) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }
  pentry.dwSize = sizeof(PROCESSENTRY32W);
  ok = Process32FirstW(hSnapShot, &pentry);
  if (! ok) {
    CloseHandle(hSnapShot);
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }
  while (ok) {
    if (pentry.th32ProcessID == pid) {
      CloseHandle(hSnapShot);
      return ScalarString(ps__utf16_to_charsxp(pentry.szExeFile, -1));
    }
    ok = Process32NextW(hSnapShot, &pentry);
  }

  CloseHandle(hSnapShot);
  ps__no_such_process("");
  ps__throw_error();
  return R_NilValue;
}


/*
 * Return process memory information as a Python tuple.
 */
SEXP ps__proc_memory_info(SEXP r_pid) {
  HANDLE hProcess;
  DWORD pid = INTEGER(r_pid)[0];
#if (_WIN32_WINNT >= 0x0501)  // Windows XP with SP2
  PROCESS_MEMORY_COUNTERS_EX cnt;
#else
  PROCESS_MEMORY_COUNTERS cnt;
#endif
  SIZE_T private = 0;

  hProcess = ps__handle_from_pid(pid);
  if (NULL == hProcess)
    ps__throw_error();

  if (! GetProcessMemoryInfo(hProcess, (PPROCESS_MEMORY_COUNTERS)&cnt,
			     sizeof(cnt))) {
    CloseHandle(hProcess);
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

#if (_WIN32_WINNT >= 0x0501)  // Windows XP with SP2
  private = cnt.PrivateUsage;
#endif

  CloseHandle(hProcess);

  // PROCESS_MEMORY_COUNTERS values are defined as SIZE_T which on 64bits
  // is an (unsigned long long) and on 32bits is an (unsigned int).
  // "_WIN64" is defined if we're running a 64bit Python interpreter not
  // exclusively if the *system* is 64bit.
#if defined(_WIN64)
  return ps__build_named_list(
    "kKKKKKKKKK",
    "num_page_faults",     cnt.PageFaultCount,  // unsigned long
    "peak_wset",           (unsigned long long) cnt.PeakWorkingSetSize,
    "wset",                (unsigned long long) cnt.WorkingSetSize,
    "peak_paged_pool",     (unsigned long long) cnt.QuotaPeakPagedPoolUsage,
    "paged_pool",          (unsigned long long) cnt.QuotaPagedPoolUsage,
    "peak_non_paged_pool", (unsigned long long) cnt.QuotaPeakNonPagedPoolUsage,
    "non_paged_pool",      (unsigned long long) cnt.QuotaNonPagedPoolUsage,
    "pagefile",            (unsigned long long) cnt.PagefileUsage,
    "peak_pagefile",       (unsigned long long) cnt.PeakPagefileUsage,
    "mem_private",         (unsigned long long) private);
#else
  return ps__build_named_list(
    "kIIIIIIIII",
    "num_page_faults",     cnt.PageFaultCount,    // unsigned long
    "peak_wget",           (unsigned int) cnt.PeakWorkingSetSize,
    "wset",                (unsigned int) cnt.WorkingSetSize,
    "peak_paged_pool",     (unsigned int) cnt.QuotaPeakPagedPoolUsage,
    "paged_pool",          (unsigned int) cnt.QuotaPagedPoolUsage,
    "peak_non_paged_pool", (unsigned int) cnt.QuotaPeakNonPagedPoolUsage,
    "non_paged_pool",      (unsigned int) cnt.QuotaNonPagedPoolUsage,
    "pagefile",            (unsigned int) cnt.PagefileUsage,
    "peak_pagefile",       (unsigned int) cnt.PeakPagefileUsage,
    "mem_private",         (unsigned int) private);
#endif
}

#if (0)


/**
 * Returns the USS of the process.
 * Reference:
 * https://dxr.mozilla.org/mozilla-central/source/xpcom/base/
 *     nsMemoryReporterManager.cpp
 */
static PyObject *
ps__proc_memory_uss(PyObject *self, PyObject *args)
{
  DWORD pid;
  HANDLE proc;
  PSAPI_WORKING_SET_INFORMATION tmp;
  DWORD tmp_size = sizeof(tmp);
  size_t entries;
  size_t private_pages;
  size_t i;
  DWORD info_array_size;
  PSAPI_WORKING_SET_INFORMATION* info_array;
  SYSTEM_INFO system_info;
  PyObject* py_result = NULL;
  unsigned long long total = 0;

  if (! PyArg_ParseTuple(args, "l", &pid))
    return NULL;

  proc = ps__handle_from_pid(pid);
  if (proc == NULL)
    return NULL;

  // Determine how many entries we need.
  memset(&tmp, 0, tmp_size);
  if (!QueryWorkingSet(proc, &tmp, tmp_size)) {
    // NB: QueryWorkingSet is expected to fail here due to the
    // buffer being too small.
    if (tmp.NumberOfEntries == 0) {
      PyErr_SetFromWindowsErr(0);
      goto done;
    }
  }

  // Fudge the size in case new entries are added between calls.
  entries = tmp.NumberOfEntries * 2;

  if (!entries) {
    goto done;
  }

  info_array_size = tmp_size + \
    ((DWORD)entries * sizeof(PSAPI_WORKING_SET_BLOCK));
  info_array = (PSAPI_WORKING_SET_INFORMATION*)malloc(info_array_size);
  if (!info_array) {
    PyErr_NoMemory();
    goto done;
  }

  if (!QueryWorkingSet(proc, info_array, info_array_size)) {
    PyErr_SetFromWindowsErr(0);
    goto done;
  }

  entries = (size_t)info_array->NumberOfEntries;
  private_pages = 0;
  for (i = 0; i < entries; i++) {
    // Count shared pages that only one process is using as private.
    if (!info_array->WorkingSetInfo[i].Shared ||
	info_array->WorkingSetInfo[i].ShareCount <= 1) {
      private_pages++;
    }
  }

  // GetSystemInfo has no return value.
  GetSystemInfo(&system_info);
  total = private_pages * system_info.dwPageSize;
  py_result = Py_BuildValue("K", total);

 done:
  if (proc) {
    CloseHandle(proc);
  }

  if (info_array) {
    free(info_array);
  }

  return py_result;
}


/*
 * Return a Python integer indicating the total amount of physical memory
 * in bytes.
 */
static PyObject *
ps__virtual_mem(PyObject *self, PyObject *args) {
  MEMORYSTATUSEX memInfo;
  memInfo.dwLength = sizeof(MEMORYSTATUSEX);

  if (! GlobalMemoryStatusEx(&memInfo))
    return PyErr_SetFromWindowsErr(0);
  return Py_BuildValue("(LLLLLL)",
		       memInfo.ullTotalPhys,      // total
		       memInfo.ullAvailPhys,      // avail
		       memInfo.ullTotalPageFile,  // total page file
		       memInfo.ullAvailPageFile,  // avail page file
		       memInfo.ullTotalVirtual,   // total virtual
		       memInfo.ullAvailVirtual);  // avail virtual
}

/*
 * Retrieves system CPU timing information as a (user, system, idle)
 * tuple. On a multiprocessor system, the values returned are the
 * sum of the designated times across all processors.
 */
static PyObject *
ps__cpu_times(PyObject *self, PyObject *args) {
  double idle, kernel, user, system;
  FILETIME idle_time, kernel_time, user_time;

  if (!GetSystemTimes(&idle_time, &kernel_time, &user_time))
    return PyErr_SetFromWindowsErr(0);

  idle = (double)((HI_T * idle_time.dwHighDateTime) + \
		  (LO_T * idle_time.dwLowDateTime));
  user = (double)((HI_T * user_time.dwHighDateTime) + \
		  (LO_T * user_time.dwLowDateTime));
  kernel = (double)((HI_T * kernel_time.dwHighDateTime) + \
		    (LO_T * kernel_time.dwLowDateTime));

  // Kernel time includes idle time.
  // We return only busy kernel time subtracting idle time from
  // kernel time.
  system = (kernel - idle);
  return Py_BuildValue("(ddd)", user, system, idle);
}


/*
 * Same as above but for all system CPUs.
 */
static PyObject *
ps__per_cpu_times(PyObject *self, PyObject *args) {
  // NtQuerySystemInformation stuff
  typedef DWORD (_stdcall * NTQSI_PROC) (int, PVOID, ULONG, PULONG);
  NTQSI_PROC NtQuerySystemInformation;
  HINSTANCE hNtDll;

  double idle, kernel, systemt, user, interrupt, dpc;
  NTSTATUS status;
  _SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION *sppi = NULL;
  UINT i;
  unsigned int ncpus;
  PyObject *py_tuple = NULL;
  PyObject *py_retlist = PyList_New(0);

  if (py_retlist == NULL)
    return NULL;

  // obtain NtQuerySystemInformation
  hNtDll = LoadLibrary(TEXT("ntdll.dll"));
  if (hNtDll == NULL) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }
  NtQuerySystemInformation = (NTQSI_PROC)GetProcAddress(
							hNtDll, "NtQuerySystemInformation");
  if (NtQuerySystemInformation == NULL) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }

  // retrieves number of processors
  ncpus = ps__get_num_cpus(1);
  if (ncpus == 0)
    goto error;

  // allocates an array of _SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION
  // structures, one per processor
  sppi = (_SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION *) \
    malloc(ncpus * sizeof(_SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION));
  if (sppi == NULL) {
    PyErr_NoMemory();
    goto error;
  }

  // gets cpu time informations
  status = NtQuerySystemInformation(
				    SystemProcessorPerformanceInformation,
				    sppi,
				    ncpus * sizeof(_SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION),
				    NULL);
  if (status != 0) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }

  // computes system global times summing each
  // processor value
  idle = user = kernel = interrupt = dpc = 0;
  for (i = 0; i < ncpus; i++) {
    py_tuple = NULL;
    user = (double)((HI_T * sppi[i].UserTime.HighPart) +
		    (LO_T * sppi[i].UserTime.LowPart));
    idle = (double)((HI_T * sppi[i].IdleTime.HighPart) +
		    (LO_T * sppi[i].IdleTime.LowPart));
    kernel = (double)((HI_T * sppi[i].KernelTime.HighPart) +
		      (LO_T * sppi[i].KernelTime.LowPart));
    interrupt = (double)((HI_T * sppi[i].InterruptTime.HighPart) +
			 (LO_T * sppi[i].InterruptTime.LowPart));
    dpc = (double)((HI_T * sppi[i].DpcTime.HighPart) +
		   (LO_T * sppi[i].DpcTime.LowPart));

    // kernel time includes idle time on windows
    // we return only busy kernel time subtracting
    // idle time from kernel time
    systemt = kernel - idle;
    py_tuple = Py_BuildValue(
			     "(ddddd)",
			     user,
			     systemt,
			     idle,
			     interrupt,
			     dpc
			     );
    if (!py_tuple)
      goto error;
    if (PyList_Append(py_retlist, py_tuple))
      goto error;
    Py_DECREF(py_tuple);
  }

  free(sppi);
  FreeLibrary(hNtDll);
  return py_retlist;

 error:
  Py_XDECREF(py_tuple);
  Py_DECREF(py_retlist);
  if (sppi)
    free(sppi);
  if (hNtDll)
    FreeLibrary(hNtDll);
  return NULL;
}

#endif

/*
 * Return process current working directory as a Python string.
 */
SEXP ps__proc_cwd(SEXP r_pid)  {
  long pid = INTEGER(r_pid)[0];
  int pid_return;

  pid_return = ps__pid_is_running(pid);
  if (pid_return == 0) {
    ps__no_such_process("");
    ps__throw_error();
  }
  if (pid_return == -1)
    ps__throw_error();

  return ps__get_cwd(pid);
}

/*
 * Resume or suspends a process
 */
int ps__proc_suspend_or_resume(DWORD pid, int suspend) {
  // a huge thanks to http://www.codeproject.com/KB/threads/pausep.aspx
  HANDLE hThreadSnap = NULL;
  HANDLE hThread;
  THREADENTRY32  te32 = {0};

  if (pid == 0) {
    ps__access_denied("");
    return FALSE;
  }

  hThreadSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if (hThreadSnap == INVALID_HANDLE_VALUE) {
    ps__set_error_from_windows_error(0);
    return FALSE;
  }

  // Fill in the size of the structure before using it
  te32.dwSize = sizeof(THREADENTRY32);

  if (! Thread32First(hThreadSnap, &te32)) {
    ps__set_error_from_windows_error(0);
    CloseHandle(hThreadSnap);
    return FALSE;
  }

  // Walk the thread snapshot to find all threads of the process.
  // If the thread belongs to the process, add its information
  // to the display list.
  do {
    if (te32.th32OwnerProcessID == pid) {
      hThread = OpenThread(THREAD_SUSPEND_RESUME, FALSE,
			   te32.th32ThreadID);
      if (hThread == NULL) {
	ps__set_error_from_windows_error(0);
	CloseHandle(hThread);
	CloseHandle(hThreadSnap);
	return FALSE;
      }
      if (suspend == 1) {
	if (SuspendThread(hThread) == (DWORD) - 1) {
	  ps__set_error_from_windows_error(0);
	  CloseHandle(hThread);
	  CloseHandle(hThreadSnap);
	  return FALSE;
	}
      }
      else {
	if (ResumeThread(hThread) == (DWORD) - 1) {
	  ps__set_error_from_windows_error(0);
	  CloseHandle(hThread);
	  CloseHandle(hThreadSnap);
	  return FALSE;
	}
      }
      CloseHandle(hThread);
    }
  } while (Thread32Next(hThreadSnap, &te32));

  CloseHandle(hThreadSnap);
  return TRUE;
}


SEXP ps__proc_suspend(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  int suspend = 1;

  if (! ps__proc_suspend_or_resume(pid, suspend))
    ps__throw_error();

  return R_NilValue;
}


SEXP ps__proc_resume(SEXP r_pid)  {
  long pid = INTEGER(r_pid)[0];
  int suspend = 0;

  if (! ps__proc_suspend_or_resume(pid, suspend))
    ps__throw_error();

  return R_NilValue;
}

#if (0)

static PyObject *
ps__proc_threads(PyObject *self, PyObject *args) {
  HANDLE hThread;
  THREADENTRY32 te32 = {0};
  long pid;
  int pid_return;
  int rc;
  FILETIME ftDummy, ftKernel, ftUser;
  HANDLE hThreadSnap = NULL;
  PyObject *py_tuple = NULL;
  PyObject *py_retlist = PyList_New(0);

  if (py_retlist == NULL)
    return NULL;
  if (! PyArg_ParseTuple(args, "l", &pid))
    goto error;
  if (pid == 0) {
    // raise AD instead of returning 0 as procexp is able to
    // retrieve useful information somehow
    AccessDenied("");
    goto error;
  }

  pid_return = ps__pid_is_running(pid);
  if (pid_return == 0) {
    NoSuchProcess("");
    goto error;
  }
  if (pid_return == -1)
    goto error;

  hThreadSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if (hThreadSnap == INVALID_HANDLE_VALUE) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }

  // Fill in the size of the structure before using it
  te32.dwSize = sizeof(THREADENTRY32);

  if (! Thread32First(hThreadSnap, &te32)) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }

  // Walk the thread snapshot to find all threads of the process.
  // If the thread belongs to the process, increase the counter.
  do {
    if (te32.th32OwnerProcessID == pid) {
      py_tuple = NULL;
      hThread = NULL;
      hThread = OpenThread(THREAD_QUERY_INFORMATION,
			   FALSE, te32.th32ThreadID);
      if (hThread == NULL) {
	// thread has disappeared on us
	continue;
      }

      rc = GetThreadTimes(hThread, &ftDummy, &ftDummy, &ftKernel,
			  &ftUser);
      if (rc == 0) {
	PyErr_SetFromWindowsErr(0);
	goto error;
      }

      /*
       * User and kernel times are represented as a FILETIME structure
       * wich contains a 64-bit value representing the number of
       * 100-nanosecond intervals since January 1, 1601 (UTC):
       * http://msdn.microsoft.com/en-us/library/ms724284(VS.85).aspx
       * To convert it into a float representing the seconds that the
       * process has executed in user/kernel mode I borrowed the code
       * below from Python's Modules/posixmodule.c
       */
      py_tuple = Py_BuildValue(
			       "kdd",
			       te32.th32ThreadID,
			       (double)(ftUser.dwHighDateTime * 429.4967296 + \
					ftUser.dwLowDateTime * 1e-7),
			       (double)(ftKernel.dwHighDateTime * 429.4967296 + \
					ftKernel.dwLowDateTime * 1e-7));
      if (!py_tuple)
	goto error;
      if (PyList_Append(py_retlist, py_tuple))
	goto error;
      Py_DECREF(py_tuple);

      CloseHandle(hThread);
    }
  } while (Thread32Next(hThreadSnap, &te32));

  CloseHandle(hThreadSnap);
  return py_retlist;

 error:
  Py_XDECREF(py_tuple);
  Py_DECREF(py_retlist);
  if (hThread != NULL)
    CloseHandle(hThread);
  if (hThreadSnap != NULL)
    CloseHandle(hThreadSnap);
  return NULL;
}


static PyObject *
ps__proc_open_files(PyObject *self, PyObject *args) {
  long       pid;
  HANDLE     processHandle;
  DWORD      access = PROCESS_DUP_HANDLE | PROCESS_QUERY_INFORMATION;
  PyObject  *py_retlist;

  if (! PyArg_ParseTuple(args, "l", &pid))
    return NULL;

  processHandle = ps__handle_from_pid_waccess(pid, access);
  if (processHandle == NULL)
    return NULL;
  py_retlist = ps__get_open_files(pid, processHandle);
  CloseHandle(processHandle);
  if (py_retlist == NULL)
    return PyErr_SetFromWindowsErr(0);
  return py_retlist;
}

#endif

/*
  Accept a filename's drive in native  format like "\Device\HarddiskVolume1\"
  and return the corresponding drive letter (e.g. "C:\\").
  If no match is found return an empty string.
*/
SEXP ps__win32_QueryDosDevice(SEXP r_path) {
  WCHAR *lpDevicePath;
  WCHAR d = 'A';

  ps__utf8_to_utf16(CHAR(STRING_ELT(r_path, 0)), &lpDevicePath);

  while (d <= 'Z') {
    WCHAR szDeviceName[3] = {d, ':', '\0' };
    WCHAR szTarget[512] = {0};
    if (QueryDosDeviceW(szDeviceName, szTarget, 511) != 0) {
      if (wcscmp(lpDevicePath, szTarget) == 0) {
	return ScalarString(ps__utf16_to_charsxp(szDeviceName, -1));
      }
    }
    d++;
  }
  return mkString("");
}


/*
 * Return process username as a "DOMAIN//USERNAME" string.
 */
SEXP ps__proc_username(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  HANDLE processHandle = NULL;
  HANDLE tokenHandle = NULL;
  PTOKEN_USER user = NULL;
  ULONG bufferSize;
  WCHAR *name = NULL;
  WCHAR *domainName = NULL;
  ULONG nameSize;
  ULONG domainNameSize;
  SID_NAME_USE nameUse;
  SEXP ret;

  processHandle = ps__handle_from_pid_waccess(pid, PROCESS_QUERY_INFORMATION);
  if (processHandle == NULL)
    ps__throw_error();

  if (!OpenProcessToken(processHandle, TOKEN_QUERY, &tokenHandle)) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  CloseHandle(processHandle);
  processHandle = NULL;

  // Get the user SID.
  bufferSize = 0x100;
  while (1) {
    user = malloc(bufferSize);
    if (user == NULL) {
      ps__no_memory("");
      goto error;
    }
    if (!GetTokenInformation(tokenHandle, TokenUser, user, bufferSize,
			     &bufferSize))
      {
	if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	  free(user);
	  continue;
	}
	else {
	  ps__set_error_from_windows_error(0);
	  goto error;
	}
      }
    break;
  }

  CloseHandle(tokenHandle);
  tokenHandle = NULL;

  // resolve the SID to a name
  nameSize = 0x100;
  domainNameSize = 0x100;
  while (1) {
    name = malloc(nameSize * sizeof(WCHAR));
    if (name == NULL) {
      ps__no_memory("");
      goto error;
    }
    domainName = malloc(domainNameSize * sizeof(WCHAR));
    if (domainName == NULL) {
      ps__no_memory("");
      goto error;
    }
    if (!LookupAccountSidW(NULL, user->User.Sid, name, &nameSize,
			   domainName, &domainNameSize, &nameUse))
      {
	if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	  free(name);
	  free(domainName);
	  continue;
	}
	else {
	  ps__set_error_from_windows_error(0);
	  goto error;
	}
      }
    break;
  }

  PROTECT(ret = allocVector(STRSXP, 2));
  SET_STRING_ELT(ret, 0, ps__utf16_to_charsxp(domainName, -1));
  SET_STRING_ELT(ret, 1, ps__utf16_to_charsxp(name, -1));

  UNPROTECT(1);
  return ret;

 error:
  if (processHandle != NULL)
    CloseHandle(processHandle);
  if (tokenHandle != NULL)
    CloseHandle(tokenHandle);
  if (name != NULL)
    free(name);
  if (domainName != NULL)
    free(domainName);
  if (user != NULL)
    free(user);
  ps__throw_error();
  return R_NilValue;
}

#if (0)

typedef DWORD (WINAPI * _GetExtendedTcpTable)(PVOID, PDWORD, BOOL, ULONG,
                                              TCP_TABLE_CLASS, ULONG);


// https://msdn.microsoft.com/library/aa365928.aspx
static DWORD __GetExtendedTcpTable(_GetExtendedTcpTable call,
                                   ULONG address_family,
                                   PVOID * data, DWORD * size)
{
  // Due to other processes being active on the machine, it's possible
  // that the size of the table increases between the moment where we
  // query the size and the moment where we query the data.  Therefore, it's
  // important to call this in a loop to retry if that happens.
  //
  // Also, since we may loop a theoretically unbounded number of times here,
  // release the GIL while we're doing this.
  DWORD error = ERROR_INSUFFICIENT_BUFFER;
  *size = 0;
  *data = NULL;
  Py_BEGIN_ALLOW_THREADS;
  error = call(NULL, size, FALSE, address_family,
	       TCP_TABLE_OWNER_PID_ALL, 0);
  while (error == ERROR_INSUFFICIENT_BUFFER)
    {
      *data = malloc(*size);
      if (*data == NULL) {
	error = ERROR_NOT_ENOUGH_MEMORY;
	continue;
      }
      error = call(*data, size, FALSE, address_family,
		   TCP_TABLE_OWNER_PID_ALL, 0);
      if (error != NO_ERROR) {
	free(*data);
	*data = NULL;
      }
    }
  Py_END_ALLOW_THREADS;
  return error;
}


typedef DWORD (WINAPI * _GetExtendedUdpTable)(PVOID, PDWORD, BOOL, ULONG,
                                              UDP_TABLE_CLASS, ULONG);


// https://msdn.microsoft.com/library/aa365930.aspx
static DWORD __GetExtendedUdpTable(_GetExtendedUdpTable call,
                                   ULONG address_family,
                                   PVOID * data, DWORD * size)
{
  // Due to other processes being active on the machine, it's possible
  // that the size of the table increases between the moment where we
  // query the size and the moment where we query the data.  Therefore, it's
  // important to call this in a loop to retry if that happens.
  //
  // Also, since we may loop a theoretically unbounded number of times here,
  // release the GIL while we're doing this.
  DWORD error = ERROR_INSUFFICIENT_BUFFER;
  *size = 0;
  *data = NULL;
  Py_BEGIN_ALLOW_THREADS;
  error = call(NULL, size, FALSE, address_family,
	       UDP_TABLE_OWNER_PID, 0);
  while (error == ERROR_INSUFFICIENT_BUFFER)
    {
      *data = malloc(*size);
      if (*data == NULL) {
	error = ERROR_NOT_ENOUGH_MEMORY;
	continue;
      }
      error = call(*data, size, FALSE, address_family,
		   UDP_TABLE_OWNER_PID, 0);
      if (error != NO_ERROR) {
	free(*data);
	*data = NULL;
      }
    }
  Py_END_ALLOW_THREADS;
  return error;
}


/*
 * Return a list of network connections opened by a process
 */
static PyObject *
ps__net_connections(PyObject *self, PyObject *args) {
  static long null_address[4] = { 0, 0, 0, 0 };
  unsigned long pid;
  int pid_return;
  typedef PSTR (NTAPI * _RtlIpv4AddressToStringA)(struct in_addr *, PSTR);
  _RtlIpv4AddressToStringA rtlIpv4AddressToStringA;
  typedef PSTR (NTAPI * _RtlIpv6AddressToStringA)(struct in6_addr *, PSTR);
  _RtlIpv6AddressToStringA rtlIpv6AddressToStringA;
  _GetExtendedTcpTable getExtendedTcpTable;
  _GetExtendedUdpTable getExtendedUdpTable;
  PVOID table = NULL;
  DWORD tableSize;
  DWORD error;
  PMIB_TCPTABLE_OWNER_PID tcp4Table;
  PMIB_UDPTABLE_OWNER_PID udp4Table;
  PMIB_TCP6TABLE_OWNER_PID tcp6Table;
  PMIB_UDP6TABLE_OWNER_PID udp6Table;
  ULONG i;
  CHAR addressBufferLocal[65];
  CHAR addressBufferRemote[65];

  PyObject *py_retlist;
  PyObject *py_conn_tuple = NULL;
  PyObject *py_af_filter = NULL;
  PyObject *py_type_filter = NULL;
  PyObject *py_addr_tuple_local = NULL;
  PyObject *py_addr_tuple_remote = NULL;
  PyObject *_AF_INET = PyLong_FromLong((long)AF_INET);
  PyObject *_AF_INET6 = PyLong_FromLong((long)AF_INET6);
  PyObject *_SOCK_STREAM = PyLong_FromLong((long)SOCK_STREAM);
  PyObject *_SOCK_DGRAM = PyLong_FromLong((long)SOCK_DGRAM);

  if (! PyArg_ParseTuple(args, "lOO", &pid, &py_af_filter, &py_type_filter))
    {
      return NULL;
    }

  if (!PySequence_Check(py_af_filter) || !PySequence_Check(py_type_filter)) {
    PyErr_SetString(PyExc_TypeError, "arg 2 or 3 is not a sequence");
    return NULL;
  }

  if (pid != -1) {
    pid_return = ps__pid_is_running(pid);
    if (pid_return == 0) {
      return NoSuchProcess("");
    }
    else if (pid_return == -1) {
      return NULL;
    }
  }

  // Import some functions.
  {
    HMODULE ntdll;
    HMODULE iphlpapi;

    ntdll = LoadLibrary(TEXT("ntdll.dll"));
    rtlIpv4AddressToStringA = (_RtlIpv4AddressToStringA)GetProcAddress(
								       ntdll, "RtlIpv4AddressToStringA");
    rtlIpv6AddressToStringA = (_RtlIpv6AddressToStringA)GetProcAddress(
								       ntdll, "RtlIpv6AddressToStringA");
    /* TODO: Check these two function pointers */

    iphlpapi = LoadLibrary(TEXT("iphlpapi.dll"));
    getExtendedTcpTable = (_GetExtendedTcpTable)GetProcAddress(iphlpapi,
							       "GetExtendedTcpTable");
    getExtendedUdpTable = (_GetExtendedUdpTable)GetProcAddress(iphlpapi,
							       "GetExtendedUdpTable");
    FreeLibrary(ntdll);
    FreeLibrary(iphlpapi);
  }

  if ((getExtendedTcpTable == NULL) || (getExtendedUdpTable == NULL)) {
    PyErr_SetString(PyExc_NotImplementedError,
		    "feature not supported on this Windows version");
    return NULL;
  }

  py_retlist = PyList_New(0);
  if (py_retlist == NULL) {
    return NULL;
  }

  // TCP IPv4

  if ((PySequence_Contains(py_af_filter, _AF_INET) == 1) &&
      (PySequence_Contains(py_type_filter, _SOCK_STREAM) == 1))
    {
      table = NULL;
      py_conn_tuple = NULL;
      py_addr_tuple_local = NULL;
      py_addr_tuple_remote = NULL;
      tableSize = 0;

      error = __GetExtendedTcpTable(getExtendedTcpTable,
				    AF_INET, &table, &tableSize);
      if (error == ERROR_NOT_ENOUGH_MEMORY) {
	PyErr_NoMemory();
	goto error;
      }

      if (error == NO_ERROR)
        {
	  tcp4Table = table;

	  for (i = 0; i < tcp4Table->dwNumEntries; i++)
            {
	      if (pid != -1) {
		if (tcp4Table->table[i].dwOwningPid != pid) {
		  continue;
		}
	      }

	      if (tcp4Table->table[i].dwLocalAddr != 0 ||
		  tcp4Table->table[i].dwLocalPort != 0)
                {
		  struct in_addr addr;

		  addr.S_un.S_addr = tcp4Table->table[i].dwLocalAddr;
		  rtlIpv4AddressToStringA(&addr, addressBufferLocal);
		  py_addr_tuple_local = Py_BuildValue(
						      "(si)",
						      addressBufferLocal,
						      BYTESWAP_USHORT(tcp4Table->table[i].dwLocalPort));
                }
	      else {
		py_addr_tuple_local = PyTuple_New(0);
	      }

	      if (py_addr_tuple_local == NULL)
		goto error;

	      // On Windows <= XP, remote addr is filled even if socket
	      // is in LISTEN mode in which case we just ignore it.
	      if ((tcp4Table->table[i].dwRemoteAddr != 0 ||
		   tcp4Table->table[i].dwRemotePort != 0) &&
		  (tcp4Table->table[i].dwState != MIB_TCP_STATE_LISTEN))
                {
		  struct in_addr addr;

		  addr.S_un.S_addr = tcp4Table->table[i].dwRemoteAddr;
		  rtlIpv4AddressToStringA(&addr, addressBufferRemote);
		  py_addr_tuple_remote = Py_BuildValue(
						       "(si)",
						       addressBufferRemote,
						       BYTESWAP_USHORT(tcp4Table->table[i].dwRemotePort));
                }
	      else
                {
		  py_addr_tuple_remote = PyTuple_New(0);
                }

	      if (py_addr_tuple_remote == NULL)
		goto error;

	      py_conn_tuple = Py_BuildValue(
					    "(iiiNNiI)",
					    -1,
					    AF_INET,
					    SOCK_STREAM,
					    py_addr_tuple_local,
					    py_addr_tuple_remote,
					    tcp4Table->table[i].dwState,
					    tcp4Table->table[i].dwOwningPid);
	      if (!py_conn_tuple)
		goto error;
	      if (PyList_Append(py_retlist, py_conn_tuple))
		goto error;
	      Py_DECREF(py_conn_tuple);
            }
        }
      else {
	PyErr_SetFromWindowsErr(error);
	goto error;
      }

      free(table);
      table = NULL;
      tableSize = 0;
    }

  // TCP IPv6
  if ((PySequence_Contains(py_af_filter, _AF_INET6) == 1) &&
      (PySequence_Contains(py_type_filter, _SOCK_STREAM) == 1))
    {
      table = NULL;
      py_conn_tuple = NULL;
      py_addr_tuple_local = NULL;
      py_addr_tuple_remote = NULL;
      tableSize = 0;

      error = __GetExtendedTcpTable(getExtendedTcpTable,
				    AF_INET6, &table, &tableSize);
      if (error == ERROR_NOT_ENOUGH_MEMORY) {
	PyErr_NoMemory();
	goto error;
      }

      if (error == NO_ERROR)
        {
	  tcp6Table = table;

	  for (i = 0; i < tcp6Table->dwNumEntries; i++)
            {
	      if (pid != -1) {
		if (tcp6Table->table[i].dwOwningPid != pid) {
		  continue;
		}
	      }

	      if (memcmp(tcp6Table->table[i].ucLocalAddr, null_address, 16)
		  != 0 || tcp6Table->table[i].dwLocalPort != 0)
                {
		  struct in6_addr addr;

		  memcpy(&addr, tcp6Table->table[i].ucLocalAddr, 16);
		  rtlIpv6AddressToStringA(&addr, addressBufferLocal);
		  py_addr_tuple_local = Py_BuildValue(
						      "(si)",
						      addressBufferLocal,
						      BYTESWAP_USHORT(tcp6Table->table[i].dwLocalPort));
                }
	      else {
		py_addr_tuple_local = PyTuple_New(0);
	      }

	      if (py_addr_tuple_local == NULL)
		goto error;

	      // On Windows <= XP, remote addr is filled even if socket
	      // is in LISTEN mode in which case we just ignore it.
	      if ((memcmp(tcp6Table->table[i].ucRemoteAddr, null_address, 16)
		   != 0 ||
		   tcp6Table->table[i].dwRemotePort != 0) &&
		  (tcp6Table->table[i].dwState != MIB_TCP_STATE_LISTEN))
                {
		  struct in6_addr addr;

		  memcpy(&addr, tcp6Table->table[i].ucRemoteAddr, 16);
		  rtlIpv6AddressToStringA(&addr, addressBufferRemote);
		  py_addr_tuple_remote = Py_BuildValue(
						       "(si)",
						       addressBufferRemote,
						       BYTESWAP_USHORT(tcp6Table->table[i].dwRemotePort));
                }
	      else {
		py_addr_tuple_remote = PyTuple_New(0);
	      }

	      if (py_addr_tuple_remote == NULL)
		goto error;

	      py_conn_tuple = Py_BuildValue(
					    "(iiiNNiI)",
					    -1,
					    AF_INET6,
					    SOCK_STREAM,
					    py_addr_tuple_local,
					    py_addr_tuple_remote,
					    tcp6Table->table[i].dwState,
					    tcp6Table->table[i].dwOwningPid);
	      if (!py_conn_tuple)
		goto error;
	      if (PyList_Append(py_retlist, py_conn_tuple))
		goto error;
	      Py_DECREF(py_conn_tuple);
            }
        }
      else {
	PyErr_SetFromWindowsErr(error);
	goto error;
      }

      free(table);
      table = NULL;
      tableSize = 0;
    }

  // UDP IPv4

  if ((PySequence_Contains(py_af_filter, _AF_INET) == 1) &&
      (PySequence_Contains(py_type_filter, _SOCK_DGRAM) == 1))
    {
      table = NULL;
      py_conn_tuple = NULL;
      py_addr_tuple_local = NULL;
      py_addr_tuple_remote = NULL;
      tableSize = 0;
      error = __GetExtendedUdpTable(getExtendedUdpTable,
				    AF_INET, &table, &tableSize);
      if (error == ERROR_NOT_ENOUGH_MEMORY) {
	PyErr_NoMemory();
	goto error;
      }

      if (error == NO_ERROR)
        {
	  udp4Table = table;

	  for (i = 0; i < udp4Table->dwNumEntries; i++)
            {
	      if (pid != -1) {
		if (udp4Table->table[i].dwOwningPid != pid) {
		  continue;
		}
	      }

	      if (udp4Table->table[i].dwLocalAddr != 0 ||
		  udp4Table->table[i].dwLocalPort != 0)
                {
		  struct in_addr addr;

		  addr.S_un.S_addr = udp4Table->table[i].dwLocalAddr;
		  rtlIpv4AddressToStringA(&addr, addressBufferLocal);
		  py_addr_tuple_local = Py_BuildValue(
						      "(si)",
						      addressBufferLocal,
						      BYTESWAP_USHORT(udp4Table->table[i].dwLocalPort));
                }
	      else {
		py_addr_tuple_local = PyTuple_New(0);
	      }

	      if (py_addr_tuple_local == NULL)
		goto error;

	      py_conn_tuple = Py_BuildValue(
					    "(iiiNNiI)",
					    -1,
					    AF_INET,
					    SOCK_DGRAM,
					    py_addr_tuple_local,
					    PyTuple_New(0),
					    PS__CONN_NONE,
					    udp4Table->table[i].dwOwningPid);
	      if (!py_conn_tuple)
		goto error;
	      if (PyList_Append(py_retlist, py_conn_tuple))
		goto error;
	      Py_DECREF(py_conn_tuple);
            }
        }
      else {
	PyErr_SetFromWindowsErr(error);
	goto error;
      }

      free(table);
      table = NULL;
      tableSize = 0;
    }

  // UDP IPv6

  if ((PySequence_Contains(py_af_filter, _AF_INET6) == 1) &&
      (PySequence_Contains(py_type_filter, _SOCK_DGRAM) == 1))
    {
      table = NULL;
      py_conn_tuple = NULL;
      py_addr_tuple_local = NULL;
      py_addr_tuple_remote = NULL;
      tableSize = 0;
      error = __GetExtendedUdpTable(getExtendedUdpTable,
				    AF_INET6, &table, &tableSize);
      if (error == ERROR_NOT_ENOUGH_MEMORY) {
	PyErr_NoMemory();
	goto error;
      }

      if (error == NO_ERROR)
        {
	  udp6Table = table;

	  for (i = 0; i < udp6Table->dwNumEntries; i++) {
	    if (pid != -1) {
	      if (udp6Table->table[i].dwOwningPid != pid) {
		continue;
	      }
	    }

	    if (memcmp(udp6Table->table[i].ucLocalAddr, null_address, 16)
		!= 0 || udp6Table->table[i].dwLocalPort != 0)
	      {
		struct in6_addr addr;

		memcpy(&addr, udp6Table->table[i].ucLocalAddr, 16);
		rtlIpv6AddressToStringA(&addr, addressBufferLocal);
		py_addr_tuple_local = Py_BuildValue(
						    "(si)",
						    addressBufferLocal,
						    BYTESWAP_USHORT(udp6Table->table[i].dwLocalPort));
	      }
	    else {
	      py_addr_tuple_local = PyTuple_New(0);
	    }

	    if (py_addr_tuple_local == NULL)
	      goto error;

	    py_conn_tuple = Py_BuildValue(
					  "(iiiNNiI)",
					  -1,
					  AF_INET6,
					  SOCK_DGRAM,
					  py_addr_tuple_local,
					  PyTuple_New(0),
					  PS__CONN_NONE,
					  udp6Table->table[i].dwOwningPid);
	    if (!py_conn_tuple)
	      goto error;
	    if (PyList_Append(py_retlist, py_conn_tuple))
	      goto error;
	    Py_DECREF(py_conn_tuple);
	  }
        }
      else {
	PyErr_SetFromWindowsErr(error);
	goto error;
      }

      free(table);
      table = NULL;
      tableSize = 0;
    }


  return py_retlist;

 error:
  Py_XDECREF(py_conn_tuple);
  Py_XDECREF(py_addr_tuple_local);
  Py_XDECREF(py_addr_tuple_remote);
  Py_DECREF(py_retlist);
  if (table != NULL)
    free(table);
  return NULL;
}


/*
 * Get process priority as a Python integer.
 */
static PyObject *
ps__proc_priority_get(PyObject *self, PyObject *args) {
  long pid;
  DWORD priority;
  HANDLE hProcess;

  if (! PyArg_ParseTuple(args, "l", &pid))
    return NULL;
  hProcess = ps__handle_from_pid(pid);
  if (hProcess == NULL)
    return NULL;
  priority = GetPriorityClass(hProcess);
  CloseHandle(hProcess);
  if (priority == 0)
    return PyErr_SetFromWindowsErr(0);
  return Py_BuildValue("i", priority);
}


/*
 * Set process priority.
 */
static PyObject *
ps__proc_priority_set(PyObject *self, PyObject *args) {
  long pid;
  int priority;
  int retval;
  HANDLE hProcess;
  DWORD access = PROCESS_QUERY_INFORMATION | PROCESS_SET_INFORMATION;

  if (! PyArg_ParseTuple(args, "li", &pid, &priority))
    return NULL;
  hProcess = ps__handle_from_pid_waccess(pid, access);
  if (hProcess == NULL)
    return NULL;
  retval = SetPriorityClass(hProcess, priority);
  CloseHandle(hProcess);
  if (retval == 0)
    return PyErr_SetFromWindowsErr(0);
  Py_RETURN_NONE;
}


#if (_WIN32_WINNT >= 0x0600)  // Windows Vista
/*
 * Get process IO priority as a Python integer.
 */
static PyObject *
ps__proc_io_priority_get(PyObject *self, PyObject *args) {
  long pid;
  HANDLE hProcess;
  PULONG IoPriority;

  _NtQueryInformationProcess NtQueryInformationProcess =
    (_NtQueryInformationProcess)GetProcAddress(
					       GetModuleHandleA("ntdll.dll"), "NtQueryInformationProcess");

  if (! PyArg_ParseTuple(args, "l", &pid))
    return NULL;
  hProcess = ps__handle_from_pid(pid);
  if (hProcess == NULL)
    return NULL;

  NtQueryInformationProcess(
			    hProcess,
			    ProcessIoPriority,
			    &IoPriority,
			    sizeof(ULONG),
			    NULL
			    );
  CloseHandle(hProcess);
  return Py_BuildValue("i", IoPriority);
}


/*
 * Set process IO priority.
 */
static PyObject *
ps__proc_io_priority_set(PyObject *self, PyObject *args) {
  long pid;
  int prio;
  HANDLE hProcess;

  _NtSetInformationProcess NtSetInformationProcess =
    (_NtSetInformationProcess)GetProcAddress(
					     GetModuleHandleA("ntdll.dll"), "NtSetInformationProcess");

  if (NtSetInformationProcess == NULL) {
    PyErr_SetString(PyExc_RuntimeError,
		    "couldn't get NtSetInformationProcess syscall");
    return NULL;
  }

  if (! PyArg_ParseTuple(args, "li", &pid, &prio))
    return NULL;
  hProcess = ps__handle_from_pid_waccess(pid, PROCESS_ALL_ACCESS);
  if (hProcess == NULL)
    return NULL;

  NtSetInformationProcess(
			  hProcess,
			  ProcessIoPriority,
			  (PVOID)&prio,
			  sizeof((PVOID)prio)
			  );

  CloseHandle(hProcess);
  Py_RETURN_NONE;
}
#endif


/*
 * Return a Python tuple referencing process I/O counters.
 */
static PyObject *
ps__proc_io_counters(PyObject *self, PyObject *args) {
  DWORD pid;
  HANDLE hProcess;
  IO_COUNTERS IoCounters;

  if (! PyArg_ParseTuple(args, "l", &pid))
    return NULL;
  hProcess = ps__handle_from_pid(pid);
  if (NULL == hProcess)
    return NULL;
  if (! GetProcessIoCounters(hProcess, &IoCounters)) {
    CloseHandle(hProcess);
    return PyErr_SetFromWindowsErr(0);
  }
  CloseHandle(hProcess);
  return Py_BuildValue("(KKKKKK)",
		       IoCounters.ReadOperationCount,
		       IoCounters.WriteOperationCount,
		       IoCounters.ReadTransferCount,
		       IoCounters.WriteTransferCount,
		       IoCounters.OtherOperationCount,
		       IoCounters.OtherTransferCount);
}


/*
 * Return process CPU affinity as a bitmask
 */
static PyObject *
ps__proc_cpu_affinity_get(PyObject *self, PyObject *args) {
  DWORD pid;
  HANDLE hProcess;
  DWORD_PTR proc_mask;
  DWORD_PTR system_mask;

  if (! PyArg_ParseTuple(args, "l", &pid))
    return NULL;
  hProcess = ps__handle_from_pid(pid);
  if (hProcess == NULL) {
    return NULL;
  }
  if (GetProcessAffinityMask(hProcess, &proc_mask, &system_mask) == 0) {
    CloseHandle(hProcess);
    return PyErr_SetFromWindowsErr(0);
  }

  CloseHandle(hProcess);
#ifdef _WIN64
  return Py_BuildValue("K", (unsigned long long)proc_mask);
#else
  return Py_BuildValue("k", (unsigned long)proc_mask);
#endif
}


/*
 * Set process CPU affinity
 */
static PyObject *
ps__proc_cpu_affinity_set(PyObject *self, PyObject *args) {
  DWORD pid;
  HANDLE hProcess;
  DWORD dwDesiredAccess = \
    PROCESS_QUERY_INFORMATION | PROCESS_SET_INFORMATION;
  DWORD_PTR mask;

#ifdef _WIN64
  if (! PyArg_ParseTuple(args, "lK", &pid, &mask))
#else
    if (! PyArg_ParseTuple(args, "lk", &pid, &mask))
#endif
      {
        return NULL;
      }
  hProcess = ps__handle_from_pid_waccess(pid, dwDesiredAccess);
  if (hProcess == NULL)
    return NULL;

  if (SetProcessAffinityMask(hProcess, mask) == 0) {
    CloseHandle(hProcess);
    return PyErr_SetFromWindowsErr(0);
  }

  CloseHandle(hProcess);
  Py_RETURN_NONE;
}

#endif

/*
 * Return True if one of the process threads is in a waiting or
 * suspended status.
 */
SEXP ps__proc_is_suspended(SEXP r_pid) {
  DWORD pid = INTEGER(r_pid)[0];
  ULONG i;
  PSYSTEM_PROCESS_INFORMATION process;
  PVOID buffer;

  if (! ps__get_proc_info(pid, &process, &buffer)) {
    ps__throw_error();
  }
  for (i = 0; i < process->NumberOfThreads; i++) {
    if (process->Threads[i].ThreadState != Waiting ||
	process->Threads[i].WaitReason != Suspended)
      {
	free(buffer);
	return ScalarLogical(FALSE);
      }
  }
  free(buffer);
  return ScalarLogical(TRUE);
}

#if (0)

/*
 * Return path's disk total and free as a Python tuple.
 */
static PyObject *
ps__disk_usage(PyObject *self, PyObject *args) {
  BOOL retval;
  ULARGE_INTEGER _, total, free;
  char *path;

  if (PyArg_ParseTuple(args, "u", &path)) {
    Py_BEGIN_ALLOW_THREADS
      retval = GetDiskFreeSpaceExW((LPCWSTR)path, &_, &total, &free);
    Py_END_ALLOW_THREADS
      goto return_;
  }

  // on Python 2 we also want to accept plain strings other
  // than Unicode
#if PY_MAJOR_VERSION <= 2
  PyErr_Clear();  // drop the argument parsing error
  if (PyArg_ParseTuple(args, "s", &path)) {
    Py_BEGIN_ALLOW_THREADS
      retval = GetDiskFreeSpaceEx(path, &_, &total, &free);
    Py_END_ALLOW_THREADS
      goto return_;
  }
#endif

  return NULL;

 return_:
  if (retval == 0)
    return PyErr_SetFromWindowsErr(0);
  else
    return Py_BuildValue("(LL)", total.QuadPart, free.QuadPart);
}


/*
 * Return a Python list of named tuples with overall network I/O information
 */
static PyObject *
ps__net_io_counters(PyObject *self, PyObject *args) {
  DWORD dwRetVal = 0;

#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
  MIB_IF_ROW2 *pIfRow = NULL;
#else // Windows XP
  MIB_IFROW *pIfRow = NULL;
#endif

  PIP_ADAPTER_ADDRESSES pAddresses = NULL;
  PIP_ADAPTER_ADDRESSES pCurrAddresses = NULL;
  PyObject *py_retdict = PyDict_New();
  PyObject *py_nic_info = NULL;
  PyObject *py_nic_name = NULL;

  if (py_retdict == NULL)
    return NULL;
  pAddresses = ps__get_nic_addresses();
  if (pAddresses == NULL)
    goto error;
  pCurrAddresses = pAddresses;

  while (pCurrAddresses) {
    py_nic_name = NULL;
    py_nic_info = NULL;

#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
    pIfRow = (MIB_IF_ROW2 *) malloc(sizeof(MIB_IF_ROW2));
#else // Windows XP
    pIfRow = (MIB_IFROW *) malloc(sizeof(MIB_IFROW));
#endif

    if (pIfRow == NULL) {
      PyErr_NoMemory();
      goto error;
    }

#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
    SecureZeroMemory((PVOID)pIfRow, sizeof(MIB_IF_ROW2));
    pIfRow->InterfaceIndex = pCurrAddresses->IfIndex;
    dwRetVal = GetIfEntry2(pIfRow);
#else // Windows XP
    pIfRow->dwIndex = pCurrAddresses->IfIndex;
    dwRetVal = GetIfEntry(pIfRow);
#endif

    if (dwRetVal != NO_ERROR) {
      PyErr_SetString(PyExc_RuntimeError,
		      "GetIfEntry() or GetIfEntry2() syscalls failed.");
      goto error;
    }

#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
    py_nic_info = Py_BuildValue("(KKKKKKKK)",
				pIfRow->OutOctets,
				pIfRow->InOctets,
				(pIfRow->OutUcastPkts + pIfRow->OutNUcastPkts),
				(pIfRow->InUcastPkts + pIfRow->InNUcastPkts),
				pIfRow->InErrors,
				pIfRow->OutErrors,
				pIfRow->InDiscards,
				pIfRow->OutDiscards);
#else // Windows XP
    py_nic_info = Py_BuildValue("(kkkkkkkk)",
				pIfRow->dwOutOctets,
				pIfRow->dwInOctets,
				(pIfRow->dwOutUcastPkts + pIfRow->dwOutNUcastPkts),
				(pIfRow->dwInUcastPkts + pIfRow->dwInNUcastPkts),
				pIfRow->dwInErrors,
				pIfRow->dwOutErrors,
				pIfRow->dwInDiscards,
				pIfRow->dwOutDiscards);
#endif

    if (!py_nic_info)
      goto error;

    py_nic_name = PyUnicode_FromWideChar(
					 pCurrAddresses->FriendlyName,
					 wcslen(pCurrAddresses->FriendlyName));

    if (py_nic_name == NULL)
      goto error;
    if (PyDict_SetItem(py_retdict, py_nic_name, py_nic_info))
      goto error;
    Py_XDECREF(py_nic_name);
    Py_XDECREF(py_nic_info);

    free(pIfRow);
    pCurrAddresses = pCurrAddresses->Next;
  }

  free(pAddresses);
  return py_retdict;

 error:
  Py_XDECREF(py_nic_name);
  Py_XDECREF(py_nic_info);
  Py_DECREF(py_retdict);
  if (pAddresses != NULL)
    free(pAddresses);
  if (pIfRow != NULL)
    free(pIfRow);
  return NULL;
}


/*
 * Return a Python dict of tuples for disk I/O information
 */
static PyObject *
ps__disk_io_counters(PyObject *self, PyObject *args) {
  DISK_PERFORMANCE_WIN_2008 diskPerformance;
  DWORD dwSize;
  HANDLE hDevice = NULL;
  char szDevice[MAX_PATH];
  char szDeviceDisplay[MAX_PATH];
  int devNum;
  int i;
  size_t ioctrlSize;
  BOOL ret;
  PyObject *py_retdict = PyDict_New();
  PyObject *py_tuple = NULL;

  if (py_retdict == NULL)
    return NULL;
  // Apparently there's no way to figure out how many times we have
  // to iterate in order to find valid drives.
  // Let's assume 32, which is higher than 26, the number of letters
  // in the alphabet (from A:\ to Z:\).
  for (devNum = 0; devNum <= 32; ++devNum) {
    py_tuple = NULL;
    sprintf_s(szDevice, MAX_PATH, "\\\\.\\PhysicalDrive%d", devNum);
    hDevice = CreateFile(szDevice, 0, FILE_SHARE_READ | FILE_SHARE_WRITE,
			 NULL, OPEN_EXISTING, 0, NULL);
    if (hDevice == INVALID_HANDLE_VALUE)
      continue;

    // DeviceIoControl() sucks!
    i = 0;
    ioctrlSize = sizeof(diskPerformance);
    while (1) {
      i += 1;
      ret = DeviceIoControl(
			    hDevice, IOCTL_DISK_PERFORMANCE, NULL, 0, &diskPerformance,
			    ioctrlSize, &dwSize, NULL);
      if (ret != 0)
	break;  // OK!
      if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	// Retry with a bigger buffer (+ limit for retries).
	if (i <= 1024) {
	  ioctrlSize *= 2;
	  continue;
	}
      }
      else if (GetLastError() == ERROR_INVALID_FUNCTION) {
	// This happens on AppVeyor:
	// https://ci.appveyor.com/project/giampaolo/psutil/build/
	//      1364/job/ascpdi271b06jle3
	// Assume it means we're dealing with some exotic disk
	// and go on.
	ps__debug("DeviceIoControl -> ERROR_INVALID_FUNCTION; "
		     "ignore PhysicalDrive%i", devNum);
	goto next;
      }
      else if (GetLastError() == ERROR_NOT_SUPPORTED) {
	// Again, let's assume we're dealing with some exotic disk.
	ps__debug("DeviceIoControl -> ERROR_NOT_SUPPORTED; "
		     "ignore PhysicalDrive%i", devNum);
	goto next;
      }
      // XXX: it seems we should also catch ERROR_INVALID_PARAMETER:
      // https://sites.ualberta.ca/dept/aict/uts/software/openbsd/
      //     ports/4.1/i386/openafs/w-openafs-1.4.14-transarc/
      //     openafs-1.4.14/src/usd/usd_nt.c

      // XXX: we can also bump into ERROR_MORE_DATA in which case
      // (quoting doc) we're supposed to retry with a bigger buffer
      // and specify  a new "starting point", whatever it means.
      PyErr_SetFromWindowsErr(0);
      goto error;
    }

    sprintf_s(szDeviceDisplay, MAX_PATH, "PhysicalDrive%i", devNum);
    py_tuple = Py_BuildValue(
			     "(IILLKK)",
			     diskPerformance.ReadCount,
			     diskPerformance.WriteCount,
			     diskPerformance.BytesRead,
			     diskPerformance.BytesWritten,
			     // convert to ms:
			     // https://github.com/giampaolo/psutil/issues/1012
			     (unsigned long long)
			     (diskPerformance.ReadTime.QuadPart) / 10000000,
			     (unsigned long long)
			     (diskPerformance.WriteTime.QuadPart) / 10000000);
    if (!py_tuple)
      goto error;
    if (PyDict_SetItemString(py_retdict, szDeviceDisplay, py_tuple))
      goto error;
    Py_XDECREF(py_tuple);

  next:
    CloseHandle(hDevice);
  }

  return py_retdict;

 error:
  Py_XDECREF(py_tuple);
  Py_DECREF(py_retdict);
  if (hDevice != NULL)
    CloseHandle(hDevice);
  return NULL;
}


static char *ps__get_drive_type(int type) {
  switch (type) {
  case DRIVE_FIXED:
    return "fixed";
  case DRIVE_CDROM:
    return "cdrom";
  case DRIVE_REMOVABLE:
    return "removable";
  case DRIVE_UNKNOWN:
    return "unknown";
  case DRIVE_NO_ROOT_DIR:
    return "unmounted";
  case DRIVE_REMOTE:
    return "remote";
  case DRIVE_RAMDISK:
    return "ramdisk";
  default:
    return "?";
  }
}


#ifndef _ARRAYSIZE
#define _ARRAYSIZE(a) (sizeof(a)/sizeof(a[0]))
#endif


/*
 * Return disk partitions as a list of tuples such as
 * (drive_letter, drive_letter, type, "")
 */
static PyObject *
ps__disk_partitions(PyObject *self, PyObject *args) {
  DWORD num_bytes;
  char drive_strings[255];
  char *drive_letter = drive_strings;
  char mp_buf[MAX_PATH];
  char mp_path[MAX_PATH];
  int all;
  int type;
  int ret;
  unsigned int old_mode = 0;
  char opts[20];
  HANDLE mp_h;
  BOOL mp_flag= TRUE;
  LPTSTR fs_type[MAX_PATH + 1] = { 0 };
  DWORD pflags = 0;
  PyObject *py_all;
  PyObject *py_retlist = PyList_New(0);
  PyObject *py_tuple = NULL;

  if (py_retlist == NULL) {
    return NULL;
  }

  // avoid to visualize a message box in case something goes wrong
  // see https://github.com/giampaolo/psutil/issues/264
  old_mode = SetErrorMode(SEM_FAILCRITICALERRORS);

  if (! PyArg_ParseTuple(args, "O", &py_all))
    goto error;
  all = PyObject_IsTrue(py_all);

  Py_BEGIN_ALLOW_THREADS
    num_bytes = GetLogicalDriveStrings(254, drive_letter);
  Py_END_ALLOW_THREADS

    if (num_bytes == 0) {
      PyErr_SetFromWindowsErr(0);
      goto error;
    }

  while (*drive_letter != 0) {
    py_tuple = NULL;
    opts[0] = 0;
    fs_type[0] = 0;

    Py_BEGIN_ALLOW_THREADS
      type = GetDriveType(drive_letter);
    Py_END_ALLOW_THREADS

      // by default we only show hard drives and cd-roms
      if (all == 0) {
	if ((type == DRIVE_UNKNOWN) ||
	    (type == DRIVE_NO_ROOT_DIR) ||
	    (type == DRIVE_REMOTE) ||
	    (type == DRIVE_RAMDISK)) {
	  goto next;
	}
	// floppy disk: skip it by default as it introduces a
	// considerable slowdown.
	if ((type == DRIVE_REMOVABLE) &&
	    (strcmp(drive_letter, "A:\\")  == 0)) {
	  goto next;
	}
      }

    ret = GetVolumeInformation(
			       (LPCTSTR)drive_letter, NULL, _ARRAYSIZE(drive_letter),
			       NULL, NULL, &pflags, (LPTSTR)fs_type, _ARRAYSIZE(fs_type));
    if (ret == 0) {
      // We might get here in case of a floppy hard drive, in
      // which case the error is (21, "device not ready").
      // Let's pretend it didn't happen as we already have
      // the drive name and type ('removable').
      strcat_s(opts, _countof(opts), "");
      SetLastError(0);
    }
    else {
      if (pflags & FILE_READ_ONLY_VOLUME)
	strcat_s(opts, _countof(opts), "ro");
      else
	strcat_s(opts, _countof(opts), "rw");
      if (pflags & FILE_VOLUME_IS_COMPRESSED)
	strcat_s(opts, _countof(opts), ",compressed");

      // Check for mount points on this volume and add/get info
      // (checks first to know if we can even have mount points)
      if (pflags & FILE_SUPPORTS_REPARSE_POINTS) {

	mp_h = FindFirstVolumeMountPoint(drive_letter, mp_buf, MAX_PATH);
	if (mp_h != INVALID_HANDLE_VALUE) {
	  while (mp_flag) {

	    // Append full mount path with drive letter
	    strcpy_s(mp_path, _countof(mp_path), drive_letter);
	    strcat_s(mp_path, _countof(mp_path), mp_buf);

	    py_tuple = Py_BuildValue(
				     "(ssss)",
				     drive_letter,
				     mp_path,
				     fs_type, // Typically NTFS
				     opts);

	    if (!py_tuple || PyList_Append(py_retlist, py_tuple) == -1) {
	      FindVolumeMountPointClose(mp_h);
	      goto error;
	    }

	    Py_DECREF(py_tuple);

	    // Continue looking for more mount points
	    mp_flag = FindNextVolumeMountPoint(mp_h, mp_buf, MAX_PATH);
	  }
	  FindVolumeMountPointClose(mp_h);
	}

      }
    }

    if (strlen(opts) > 0)
      strcat_s(opts, _countof(opts), ",");
    strcat_s(opts, _countof(opts), ps__get_drive_type(type));

    py_tuple = Py_BuildValue(
			     "(ssss)",
			     drive_letter,
			     drive_letter,
			     fs_type,  // either FAT, FAT32, NTFS, HPFS, CDFS, UDF or NWFS
			     opts);
    if (!py_tuple)
      goto error;
    if (PyList_Append(py_retlist, py_tuple))
      goto error;
    Py_DECREF(py_tuple);
    goto next;

  next:
    drive_letter = strchr(drive_letter, 0) + 1;
  }

  SetErrorMode(old_mode);
  return py_retlist;

 error:
  SetErrorMode(old_mode);
  Py_XDECREF(py_tuple);
  Py_DECREF(py_retlist);
  return NULL;
}

/*
 * Return a Python dict of tuples for disk I/O information
 */
static PyObject *
ps__users(PyObject *self, PyObject *args) {
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
  long long unix_time;

  PWINSTATIONQUERYINFORMATIONW WinStationQueryInformationW;
  WINSTATION_INFO station_info;
  HINSTANCE hInstWinSta = NULL;
  ULONG returnLen;

  PyObject *py_retlist = PyList_New(0);
  PyObject *py_tuple = NULL;
  PyObject *py_address = NULL;
  PyObject *py_username = NULL;

  if (py_retlist == NULL)
    return NULL;

  hInstWinSta = LoadLibraryA("winsta.dll");
  WinStationQueryInformationW = (PWINSTATIONQUERYINFORMATIONW) \
    GetProcAddress(hInstWinSta, "WinStationQueryInformationW");

  if (WTSEnumerateSessions(hServer, 0, 1, &sessions, &count) == 0) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }

  for (i = 0; i < count; i++) {
    py_address = NULL;
    py_tuple = NULL;
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
      PyErr_SetFromWindowsErr(0);
      goto error;
    }
    if (bytes <= 2)
      continue;

    // address
    bytes = 0;
    if (WTSQuerySessionInformation(hServer, sessionId, WTSClientAddress,
				   &buffer_addr, &bytes) == 0) {
      PyErr_SetFromWindowsErr(0);
      goto error;
    }

    address = (PWTS_CLIENT_ADDRESS)buffer_addr;
    if (address->AddressFamily == 0) {  // AF_INET
      sprintf_s(address_str,
		_countof(address_str),
		"%u.%u.%u.%u",
		address->Address[0],
		address->Address[1],
		address->Address[2],
		address->Address[3]);
      py_address = Py_BuildValue("s", address_str);
      if (!py_address)
	goto error;
    }
    else {
      py_address = Py_None;
    }

    // login time
    if (!WinStationQueryInformationW(hServer,
				     sessionId,
				     WinStationInformation,
				     &station_info,
				     sizeof(station_info),
				     &returnLen))
      {
	goto error;
      }

    unix_time = ((LONGLONG)station_info.ConnectTime.dwHighDateTime) << 32;
    unix_time += \
      station_info.ConnectTime.dwLowDateTime - 116444736000000000LL;
    unix_time /= 10000000;

    py_username = PyUnicode_FromWideChar(buffer_user, wcslen(buffer_user));
    if (py_username == NULL)
      goto error;
    py_tuple = Py_BuildValue("OOd",
			     py_username,
			     py_address,
			     (double)unix_time);
    if (!py_tuple)
      goto error;
    if (PyList_Append(py_retlist, py_tuple))
      goto error;
    Py_XDECREF(py_username);
    Py_XDECREF(py_address);
    Py_XDECREF(py_tuple);
  }

  WTSFreeMemory(sessions);
  WTSFreeMemory(buffer_user);
  WTSFreeMemory(buffer_addr);
  FreeLibrary(hInstWinSta);
  return py_retlist;

 error:
  Py_XDECREF(py_username);
  Py_XDECREF(py_tuple);
  Py_XDECREF(py_address);
  Py_DECREF(py_retlist);

  if (hInstWinSta != NULL)
    FreeLibrary(hInstWinSta);
  if (sessions != NULL)
    WTSFreeMemory(sessions);
  if (buffer_user != NULL)
    WTSFreeMemory(buffer_user);
  if (buffer_addr != NULL)
    WTSFreeMemory(buffer_addr);
  return NULL;
}


/*
 * Return the number of handles opened by process.
 */
static PyObject *
ps__proc_num_handles(PyObject *self, PyObject *args) {
  DWORD pid;
  HANDLE hProcess;
  DWORD handleCount;

  if (! PyArg_ParseTuple(args, "l", &pid))
    return NULL;
  hProcess = ps__handle_from_pid(pid);
  if (NULL == hProcess)
    return NULL;
  if (! GetProcessHandleCount(hProcess, &handleCount)) {
    CloseHandle(hProcess);
    return PyErr_SetFromWindowsErr(0);
  }
  CloseHandle(hProcess);
  return Py_BuildValue("k", handleCount);
}

#endif

/*
 * Get various process information by using NtQuerySystemInformation.
 * We use this as a fallback when faster functions fail with access
 * denied. This is slower because it iterates over all processes.
 * Returned tuple includes the following process info:
 *
 * - num_threads()
 * - ctx_switches()
 * - num_handles() (fallback)
 * - cpu_times() (fallback)
 * - create_time() (fallback)
 * - io_counters() (fallback)
 * - memory_info() (fallback)
 */

SEXP ps__proc_info(SEXP r_pid) {
  DWORD pid = INTEGER(r_pid)[0];
  PSYSTEM_PROCESS_INFORMATION process;
  PVOID buffer;
  ULONG i;
  ULONG ctx_switches = 0;
  double user_time;
  double kernel_time;
  long long create_time;
  SIZE_T mem_private;
  SEXP retlist;

  if (! ps__get_proc_info(pid, &process, &buffer))
    ps__throw_error();

  for (i = 0; i < process->NumberOfThreads; i++)
    ctx_switches += process->Threads[i].ContextSwitches;
  user_time = (double)process->UserTime.HighPart * HI_T + \
    (double)process->UserTime.LowPart * LO_T;
  kernel_time = (double)process->KernelTime.HighPart * HI_T + \
    (double)process->KernelTime.LowPart * LO_T;

  // Convert the LARGE_INTEGER union to a Unix time.
  // It's the best I could find by googling and borrowing code here
  // and there. The time returned has a precision of 1 second.
  if (0 == pid || 4 == pid) {
    // the python module will translate this into BOOT_TIME later
    create_time = 0;
  }
  else {
    create_time = ((LONGLONG)process->CreateTime.HighPart) << 32;
    create_time += process->CreateTime.LowPart - 116444736000000000LL;
    create_time /= 10000000;
  }

#if (_WIN32_WINNT >= 0x0501)  // Windows XP with SP2
  mem_private = process->PrivatePageCount;
#else
  mem_private = 0;
#endif

  PROTECT(retlist = ps__build_named_list(
#if defined(_WIN64)
    "kkdddiKKKKKK" "kKKKKKKKKK",
#else
    "kkdddiKKKKKK" "kIIIIIIIII",
#endif
    "num_handles",         process->HandleCount,
    "ctx_switches",        ctx_switches,
    "user_time",           user_time,
    "kernel_time",         kernel_time,
    "create_time",         (double)create_time,
    "num_threads",         (int)process->NumberOfThreads,
    // IO counters
    "io_rcount",           process->ReadOperationCount.QuadPart,
    "io_wcount",           process->WriteOperationCount.QuadPart,
    "io_rbytes",           process->ReadTransferCount.QuadPart,
    "io_wbytes",           process->WriteTransferCount.QuadPart,
    "io_count_others",     process->OtherOperationCount.QuadPart,
    "io_bytes_others",     process->OtherTransferCount.QuadPart,
    // memory
    "num_page_faults",     process->PageFaultCount,
    "peak_wset",           process->PeakWorkingSetSize,
    "wset",                process->WorkingSetSize,
    "peak_paged_pool",     process->QuotaPeakPagedPoolUsage,
    "paged_pool",          process->QuotaPagedPoolUsage,
    "peak_non_paged_pool", process->QuotaPeakNonPagedPoolUsage,
    "non_paged_oool",      process->QuotaNonPagedPoolUsage,
    "pagefile",            process->PagefileUsage,
    "peak_pagefile",       process->PeakPagefileUsage,
    "mem_private",         mem_private
  ));

  free(buffer);
  UNPROTECT(1);
  return retlist;
}

#if (0)

static char *get_region_protection_string(ULONG protection) {
  switch (protection & 0xff) {
  case PAGE_NOACCESS:
    return "";
  case PAGE_READONLY:
    return "r";
  case PAGE_READWRITE:
    return "rw";
  case PAGE_WRITECOPY:
    return "wc";
  case PAGE_EXECUTE:
    return "x";
  case PAGE_EXECUTE_READ:
    return "xr";
  case PAGE_EXECUTE_READWRITE:
    return "xrw";
  case PAGE_EXECUTE_WRITECOPY:
    return "xwc";
  default:
    return "?";
  }
}


/*
 * Return a list of process's memory mappings.
 */
static PyObject *
ps__proc_memory_maps(PyObject *self, PyObject *args) {
#ifdef _WIN64
  MEMORY_BASIC_INFORMATION64 basicInfo;
#else
  MEMORY_BASIC_INFORMATION basicInfo;
#endif
  DWORD pid;
  HANDLE hProcess = NULL;
  PVOID baseAddress;
  ULONGLONG previousAllocationBase;
  WCHAR mappedFileName[MAX_PATH];
  SYSTEM_INFO system_info;
  LPVOID maxAddr;
  PyObject *py_retlist = PyList_New(0);
  PyObject *py_tuple = NULL;
  PyObject *py_str = NULL;

  if (py_retlist == NULL)
    return NULL;
  if (! PyArg_ParseTuple(args, "l", &pid))
    goto error;
  hProcess = ps__handle_from_pid(pid);
  if (NULL == hProcess)
    goto error;

  GetSystemInfo(&system_info);
  maxAddr = system_info.lpMaximumApplicationAddress;
  baseAddress = NULL;
  previousAllocationBase = NULL;

  while (VirtualQueryEx(hProcess, baseAddress, &basicInfo,
			sizeof(MEMORY_BASIC_INFORMATION)))
    {
      py_tuple = NULL;
      if (baseAddress > maxAddr)
	break;
      if (GetMappedFileNameW(hProcess, baseAddress, mappedFileName,
			     sizeof(mappedFileName)))
        {
	  py_str = PyUnicode_FromWideChar(mappedFileName,
					  wcslen(mappedFileName));
	  if (py_str == NULL)
	    goto error;
	  py_tuple = Py_BuildValue(
#ifdef _WIN64
				   "(KsOI)",
				   (unsigned long long)baseAddress,
#else
				   "(ksOI)",
				   (unsigned long)baseAddress,
#endif
				   get_region_protection_string(basicInfo.Protect),
				   py_str,
				   basicInfo.RegionSize);

	  if (!py_tuple)
	    goto error;
	  if (PyList_Append(py_retlist, py_tuple))
	    goto error;
	  Py_DECREF(py_tuple);
	  Py_DECREF(py_str);
	}
      previousAllocationBase = basicInfo.AllocationBase;
      baseAddress = (PCHAR)baseAddress + basicInfo.RegionSize;
    }

  CloseHandle(hProcess);
  return py_retlist;

 error:
  Py_XDECREF(py_tuple);
  Py_XDECREF(py_str);
  Py_DECREF(py_retlist);
  if (hProcess != NULL)
    CloseHandle(hProcess);
  return NULL;
}

#endif

/*
 * Return a {pid:ppid, ...} dict for all running processes.
 */
SEXP ps__ppid_map() {
  SEXP ret;
  HANDLE handle = NULL;
  PROCESSENTRY32 pe = {0};
  size_t num_proc = 0, idx = 0;
  pe.dwSize = sizeof(PROCESSENTRY32);

  handle = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (handle == INVALID_HANDLE_VALUE) {
    ps__set_error_from_windows_error(0);
    ps__throw_error();
  }

  if (Process32First(handle, &pe)) {
    do {
      num_proc++;
    } while (Process32Next(handle, &pe));
  }

  PROTECT(ret = allocVector(INTSXP, 2 * num_proc));

  if (Process32First(handle, &pe)) {
    do {
      INTEGER(ret)[idx++] = pe.th32ParentProcessID;
      INTEGER(ret)[idx++] = pe.th32ProcessID;
    } while (Process32Next(handle, &pe));
  }

  CloseHandle(handle);
  UNPROTECT(1);
  return ret;
}

#if (0)

/*
 * Return NICs addresses.
 */

static PyObject *
ps__net_if_addrs(PyObject *self, PyObject *args) {
  unsigned int i = 0;
  ULONG family;
  PCTSTR intRet;
  PCTSTR netmaskIntRet;
  char *ptr;
  char buff_addr[1024];
  char buff_macaddr[1024];
  char buff_netmask[1024];
  DWORD dwRetVal = 0;
#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
  ULONG converted_netmask;
  UINT netmask_bits;
  struct in_addr in_netmask;
#endif
  PIP_ADAPTER_ADDRESSES pAddresses = NULL;
  PIP_ADAPTER_ADDRESSES pCurrAddresses = NULL;
  PIP_ADAPTER_UNICAST_ADDRESS pUnicast = NULL;

  PyObject *py_retlist = PyList_New(0);
  PyObject *py_tuple = NULL;
  PyObject *py_address = NULL;
  PyObject *py_mac_address = NULL;
  PyObject *py_nic_name = NULL;
  PyObject *py_netmask = NULL;

  if (py_retlist == NULL)
    return NULL;

  pAddresses = ps__get_nic_addresses();
  if (pAddresses == NULL)
    goto error;
  pCurrAddresses = pAddresses;

  while (pCurrAddresses) {
    pUnicast = pCurrAddresses->FirstUnicastAddress;

    netmaskIntRet = NULL;
    py_nic_name = NULL;
    py_nic_name = PyUnicode_FromWideChar(
					 pCurrAddresses->FriendlyName,
					 wcslen(pCurrAddresses->FriendlyName));
    if (py_nic_name == NULL)
      goto error;

    // MAC address
    if (pCurrAddresses->PhysicalAddressLength != 0) {
      ptr = buff_macaddr;
      *ptr = '\0';
      for (i = 0; i < (int) pCurrAddresses->PhysicalAddressLength; i++) {
	if (i == (pCurrAddresses->PhysicalAddressLength - 1)) {
	  sprintf_s(ptr, _countof(buff_macaddr), "%.2X\n",
		    (int)pCurrAddresses->PhysicalAddress[i]);
	}
	else {
	  sprintf_s(ptr, _countof(buff_macaddr), "%.2X-",
		    (int)pCurrAddresses->PhysicalAddress[i]);
	}
	ptr += 3;
      }
      *--ptr = '\0';

      py_mac_address = Py_BuildValue("s", buff_macaddr);
      if (py_mac_address == NULL)
	goto error;

      Py_INCREF(Py_None);
      Py_INCREF(Py_None);
      Py_INCREF(Py_None);
      py_tuple = Py_BuildValue(
			       "(OiOOOO)",
			       py_nic_name,
			       -1,  // this will be converted later to AF_LINK
			       py_mac_address,
			       Py_None,  // netmask (not supported)
			       Py_None,  // broadcast (not supported)
			       Py_None  // ptp (not supported on Windows)
			       );
      if (! py_tuple)
	goto error;
      if (PyList_Append(py_retlist, py_tuple))
	goto error;
      Py_DECREF(py_tuple);
      Py_DECREF(py_mac_address);
    }

    // find out the IP address associated with the NIC
    if (pUnicast != NULL) {
      for (i = 0; pUnicast != NULL; i++) {
	family = pUnicast->Address.lpSockaddr->sa_family;
	if (family == AF_INET) {
	  struct sockaddr_in *sa_in = (struct sockaddr_in *)
	    pUnicast->Address.lpSockaddr;
	  intRet = inet_ntop(AF_INET, &(sa_in->sin_addr), buff_addr,
			     sizeof(buff_addr));
	  if (!intRet)
	    goto error;
#if (_WIN32_WINNT >= 0x0600) // Windows Vista and above
	  netmask_bits = pUnicast->OnLinkPrefixLength;
	  dwRetVal = ConvertLengthToIpv4Mask(netmask_bits, &converted_netmask);
	  if (dwRetVal == NO_ERROR) {
	    in_netmask.s_addr = converted_netmask;
	    netmaskIntRet = inet_ntop(
				      AF_INET, &in_netmask, buff_netmask,
				      sizeof(buff_netmask));
	    if (!netmaskIntRet)
	      goto error;
	  }
#endif
	}
	else if (family == AF_INET6) {
	  struct sockaddr_in6 *sa_in6 = (struct sockaddr_in6 *)
	    pUnicast->Address.lpSockaddr;
	  intRet = inet_ntop(AF_INET6, &(sa_in6->sin6_addr),
			     buff_addr, sizeof(buff_addr));
	  if (!intRet)
	    goto error;
	}
	else {
	  // we should never get here
	  pUnicast = pUnicast->Next;
	  continue;
	}

#if PY_MAJOR_VERSION >= 3
	py_address = PyUnicode_FromString(buff_addr);
#else
	py_address = PyString_FromString(buff_addr);
#endif
	if (py_address == NULL)
	  goto error;

	if (netmaskIntRet != NULL) {
#if PY_MAJOR_VERSION >= 3
	  py_netmask = PyUnicode_FromString(buff_netmask);
#else
	  py_netmask = PyString_FromString(buff_netmask);
#endif
	} else {
	  Py_INCREF(Py_None);
	  py_netmask = Py_None;
	}

	Py_INCREF(Py_None);
	Py_INCREF(Py_None);
	py_tuple = Py_BuildValue(
				 "(OiOOOO)",
				 py_nic_name,
				 family,
				 py_address,
				 py_netmask,
				 Py_None,  // broadcast (not supported)
				 Py_None  // ptp (not supported on Windows)
				 );

	if (! py_tuple)
	  goto error;
	if (PyList_Append(py_retlist, py_tuple))
	  goto error;
	Py_DECREF(py_tuple);
	Py_DECREF(py_address);
	Py_DECREF(py_netmask);

	pUnicast = pUnicast->Next;
      }
    }
    Py_DECREF(py_nic_name);
    pCurrAddresses = pCurrAddresses->Next;
  }

  free(pAddresses);
  return py_retlist;

 error:
  if (pAddresses)
    free(pAddresses);
  Py_DECREF(py_retlist);
  Py_XDECREF(py_tuple);
  Py_XDECREF(py_address);
  Py_XDECREF(py_nic_name);
  Py_XDECREF(py_netmask);
  return NULL;
}


/*
 * Provides stats about NIC interfaces installed on the system.
 * TODO: get 'duplex' (currently it's hard coded to '2', aka
 'full duplex')
*/
static PyObject *
ps__net_if_stats(PyObject *self, PyObject *args) {
  int i;
  DWORD dwSize = 0;
  DWORD dwRetVal = 0;
  MIB_IFTABLE *pIfTable;
  MIB_IFROW *pIfRow;
  PIP_ADAPTER_ADDRESSES pAddresses = NULL;
  PIP_ADAPTER_ADDRESSES pCurrAddresses = NULL;
  char descr[MAX_PATH];
  int ifname_found;

  PyObject *py_nic_name = NULL;
  PyObject *py_retdict = PyDict_New();
  PyObject *py_ifc_info = NULL;
  PyObject *py_is_up = NULL;

  if (py_retdict == NULL)
    return NULL;

  pAddresses = ps__get_nic_addresses();
  if (pAddresses == NULL)
    goto error;

  pIfTable = (MIB_IFTABLE *) malloc(sizeof (MIB_IFTABLE));
  if (pIfTable == NULL) {
    PyErr_NoMemory();
    goto error;
  }
  dwSize = sizeof(MIB_IFTABLE);
  if (GetIfTable(pIfTable, &dwSize, FALSE) == ERROR_INSUFFICIENT_BUFFER) {
    free(pIfTable);
    pIfTable = (MIB_IFTABLE *) malloc(dwSize);
    if (pIfTable == NULL) {
      PyErr_NoMemory();
      goto error;
    }
  }
  // Make a second call to GetIfTable to get the actual
  // data we want.
  if ((dwRetVal = GetIfTable(pIfTable, &dwSize, FALSE)) != NO_ERROR) {
    PyErr_SetString(PyExc_RuntimeError, "GetIfTable() syscall failed");
    goto error;
  }

  for (i = 0; i < (int) pIfTable->dwNumEntries; i++) {
    pIfRow = (MIB_IFROW *) & pIfTable->table[i];

    // GetIfTable is not able to give us NIC with "friendly names"
    // so we determine them via GetAdapterAddresses() which
    // provides friendly names *and* descriptions and find the
    // ones that match.
    ifname_found = 0;
    pCurrAddresses = pAddresses;
    while (pCurrAddresses) {
      sprintf_s(descr, MAX_PATH, "%wS", pCurrAddresses->Description);
      if (lstrcmp(descr, pIfRow->bDescr) == 0) {
	py_nic_name = PyUnicode_FromWideChar(
					     pCurrAddresses->FriendlyName,
					     wcslen(pCurrAddresses->FriendlyName));
	if (py_nic_name == NULL)
	  goto error;
	ifname_found = 1;
	break;
      }
      pCurrAddresses = pCurrAddresses->Next;
    }
    if (ifname_found == 0) {
      // Name not found means GetAdapterAddresses() doesn't list
      // this NIC, only GetIfTable, meaning it's not really a NIC
      // interface so we skip it.
      continue;
    }

    // is up?
    if((pIfRow->dwOperStatus == MIB_IF_OPER_STATUS_CONNECTED ||
	pIfRow->dwOperStatus == MIB_IF_OPER_STATUS_OPERATIONAL) &&
       pIfRow->dwAdminStatus == 1 ) {
      py_is_up = Py_True;
    }
    else {
      py_is_up = Py_False;
    }
    Py_INCREF(py_is_up);

    py_ifc_info = Py_BuildValue(
				"(Oikk)",
				py_is_up,
				2,  // there's no way to know duplex so let's assume 'full'
				pIfRow->dwSpeed / 1000000,  // expressed in bytes, we want Mb
				pIfRow->dwMtu
				);
    if (!py_ifc_info)
      goto error;
    if (PyDict_SetItem(py_retdict, py_nic_name, py_ifc_info))
      goto error;
    Py_DECREF(py_nic_name);
    Py_DECREF(py_ifc_info);
  }

  free(pIfTable);
  free(pAddresses);
  return py_retdict;

 error:
  Py_XDECREF(py_is_up);
  Py_XDECREF(py_ifc_info);
  Py_XDECREF(py_nic_name);
  Py_DECREF(py_retdict);
  if (pIfTable != NULL)
    free(pIfTable);
  if (pAddresses != NULL)
    free(pAddresses);
  return NULL;
}


/*
 * Return CPU statistics.
 */
static PyObject *
ps__cpu_stats(PyObject *self, PyObject *args) {
  // NtQuerySystemInformation stuff
  typedef DWORD (_stdcall * NTQSI_PROC) (int, PVOID, ULONG, PULONG);
  NTQSI_PROC NtQuerySystemInformation;
  HINSTANCE hNtDll;

  NTSTATUS status;
  _SYSTEM_PERFORMANCE_INFORMATION *spi = NULL;
  _SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION *sppi = NULL;
  _SYSTEM_INTERRUPT_INFORMATION *InterruptInformation = NULL;
  unsigned int ncpus;
  UINT i;
  ULONG64 dpcs = 0;
  ULONG interrupts = 0;

  // obtain NtQuerySystemInformation
  hNtDll = LoadLibrary(TEXT("ntdll.dll"));
  if (hNtDll == NULL) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }
  NtQuerySystemInformation = (NTQSI_PROC)GetProcAddress(
							hNtDll, "NtQuerySystemInformation");
  if (NtQuerySystemInformation == NULL) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }

  // retrieves number of processors
  ncpus = ps__get_num_cpus(1);
  if (ncpus == 0)
    goto error;

  // get syscalls / ctx switches
  spi = (_SYSTEM_PERFORMANCE_INFORMATION *) \
    malloc(ncpus * sizeof(_SYSTEM_PERFORMANCE_INFORMATION));
  if (spi == NULL) {
    PyErr_NoMemory();
    goto error;
  }
  status = NtQuerySystemInformation(
				    SystemPerformanceInformation,
				    spi,
				    ncpus * sizeof(_SYSTEM_PERFORMANCE_INFORMATION),
				    NULL);
  if (status != 0) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }

  // get DPCs
  InterruptInformation = \
    malloc(sizeof(_SYSTEM_INTERRUPT_INFORMATION) * ncpus);
  if (InterruptInformation == NULL) {
    PyErr_NoMemory();
    goto error;
  }

  status = NtQuerySystemInformation(
				    SystemInterruptInformation,
				    InterruptInformation,
				    ncpus * sizeof(SYSTEM_INTERRUPT_INFORMATION),
				    NULL);
  if (status != 0) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }
  for (i = 0; i < ncpus; i++) {
    dpcs += InterruptInformation[i].DpcCount;
  }

  // get interrupts
  sppi = (_SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION *) \
    malloc(ncpus * sizeof(_SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION));
  if (sppi == NULL) {
    PyErr_NoMemory();
    goto error;
  }

  status = NtQuerySystemInformation(
				    SystemProcessorPerformanceInformation,
				    sppi,
				    ncpus * sizeof(_SYSTEM_PROCESSOR_PERFORMANCE_INFORMATION),
				    NULL);
  if (status != 0) {
    PyErr_SetFromWindowsErr(0);
    goto error;
  }

  for (i = 0; i < ncpus; i++) {
    interrupts += sppi[i].InterruptCount;
  }

  // done
  free(spi);
  free(InterruptInformation);
  free(sppi);
  FreeLibrary(hNtDll);
  return Py_BuildValue(
		       "kkkk",
		       spi->ContextSwitches,
		       interrupts,
		       (unsigned long)dpcs,
		       spi->SystemCalls
		       );

 error:
  if (spi)
    free(spi);
  if (InterruptInformation)
    free(InterruptInformation);
  if (sppi)
    free(sppi);
  if (hNtDll)
    FreeLibrary(hNtDll);
  return NULL;
}


/*
 * Return CPU frequency.
 */
static PyObject *
ps__cpu_freq(PyObject *self, PyObject *args) {
  PROCESSOR_POWER_INFORMATION *ppi;
  NTSTATUS ret;
  size_t size;
  LPBYTE pBuffer = NULL;
  ULONG current;
  ULONG max;
  unsigned int ncpus;

  // Get the number of CPUs.
  ncpus = ps__get_num_cpus(1);
  if (ncpus == 0)
    return NULL;

  // Allocate size.
  size = ncpus * sizeof(PROCESSOR_POWER_INFORMATION);
  pBuffer = (BYTE*)LocalAlloc(LPTR, size);
  if (! pBuffer)
    return PyErr_SetFromWindowsErr(0);

  // Syscall.
  ret = CallNtPowerInformation(
			       ProcessorInformation, NULL, 0, pBuffer, size);
  if (ret != 0) {
    PyErr_SetString(PyExc_RuntimeError,
		    "CallNtPowerInformation syscall failed");
    goto error;
  }

  // Results.
  ppi = (PROCESSOR_POWER_INFORMATION *)pBuffer;
  max = ppi->MaxMhz;
  current = ppi->CurrentMhz;
  LocalFree(pBuffer);

  return Py_BuildValue("kk", current, max);

 error:
  if (pBuffer != NULL)
    LocalFree(pBuffer);
  return NULL;
}


/*
 * Return battery usage stats.
 */
static PyObject *
ps__sensors_battery(PyObject *self, PyObject *args) {
  SYSTEM_POWER_STATUS sps;

  if (GetSystemPowerStatus(&sps) == 0)
    return PyErr_SetFromWindowsErr(0);
  return Py_BuildValue(
		       "iiiI",
		       sps.ACLineStatus,  // whether AC is connected: 0=no, 1=yes, 255=unknown
		       // status flag:
		       // 1, 2, 4 = high, low, critical
		       // 8 = charging
		       // 128 = no battery
		       sps.BatteryFlag,
		       sps.BatteryLifePercent,  // percent
		       sps.BatteryLifeTime  // remaining secs
		       );
}

#endif

SEXP ps__init(SEXP psenv, SEXP constenv) {
  return R_NilValue;
}
