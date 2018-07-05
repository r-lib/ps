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
#else
#include <winternl.h>
#include <ws2tcpip.h>
#endif

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


static ULONGLONG (*ps__GetTickCount64)(void) = NULL;

/*
 * Return a double representing the system uptime expressed in seconds
 * since the epoch.
 */
SEXP psw__boot_time() {
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
SEXP psw__pid_exists(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  int status;

  status = ps__pid_is_running(pid);
  if (-1 == status)
    ps__throw_error(); // exception raised in ps__pid_is_running()
  return ScalarLogical(status);
}


/*
 * Return an integer vector of all the PIDs running on the system.
 */
SEXP psw__pids() {
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
SEXP psw__proc_kill(SEXP r_pid) {
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
      psw__set_error_from_windows_error(0);
    }
    ps__throw_error();
  }

  // kill the process
  if (! TerminateProcess(hProcess, SIGTERM)) {
    err = GetLastError();
    // See: https://github.com/giampaolo/psutil/issues/1099
    if (err != ERROR_ACCESS_DENIED) {
      CloseHandle(hProcess);
      psw__set_error_from_windows_error(0);
      ps__throw_error();
    }
  }

  CloseHandle(hProcess);
  return R_NilValue;
}


/*
 * Return a list (user_time, kernel_time)
 */
SEXP psw__proc_cpu_times(SEXP r_pid) {
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
      psw__set_error_from_windows_error(0);
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
 * Return a double indicating the process create time expressed in
 * seconds since the epoch.
 */
SEXP psw__proc_create_time(SEXP r_pid) {
  long        pid = INTEGER(r_pid)[0];
  long long   unix_time;
  HANDLE      hProcess;
  FILETIME    ftCreate, ftExit, ftKernel, ftUser;

  // special case for PIDs 0 and 4, return system boot time
  if (0 == pid || 4 == pid)
    return psw__boot_time();

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
      psw__set_error_from_windows_error(0);
      ps__throw_error();
    }
  }

  CloseHandle(hProcess);

  // Convert the FILETIME structure to a Unix time.
  // It's the best I could find by googling and borrowing code here
  // and there. The time returned has a precision of 1 second.
  unix_time = ((LONGLONG)ftCreate.dwHighDateTime) << 32;
  unix_time += ftCreate.dwLowDateTime - 116444736000000000LL;
  unix_time /= 10000000;
  return ScalarReal((double)unix_time);
}


/*
 * Return process cmdline as a character vector of cmdline arguments.
 */
SEXP psw__proc_cmdline(SEXP r_pid) {
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
 * Return process environment variables as a character vector
 */
SEXP psw__proc_environ(SEXP r_pid) {
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
SEXP psw__proc_exe(SEXP r_pid) {
  long pid =  INTEGER(r_pid)[0];
  HANDLE hProcess;
  wchar_t exe[MAX_PATH];

  hProcess = ps__handle_from_pid_waccess(pid, PROCESS_QUERY_INFORMATION);
  if (NULL == hProcess)
    error("No  such process");

  if (GetProcessImageFileNameW(hProcess, exe, MAX_PATH) == 0) {
    CloseHandle(hProcess);
    psw__set_error_from_windows_error(0);
    ps__throw_error();
  }
  CloseHandle(hProcess);
  return ScalarString(psw__utf16_to_charsxp(exe, -1));
}

/*
 * Return process base name.
 * Note: ps__proc_exe() is attempted first because it's faster
 * but it raise AccessDenied for processes owned by other users
 * in which case we fall back on using this.
 */
SEXP psw__proc_name(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];;
  int ok;
  PROCESSENTRY32W pentry;
  HANDLE hSnapShot;

  hSnapShot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, pid);
  if (hSnapShot == INVALID_HANDLE_VALUE) {
    psw__set_error_from_windows_error(0);
    ps__throw_error();
  }
  pentry.dwSize = sizeof(PROCESSENTRY32W);
  ok = Process32FirstW(hSnapShot, &pentry);
  if (! ok) {
    CloseHandle(hSnapShot);
    psw__set_error_from_windows_error(0);
    ps__throw_error();
  }
  while (ok) {
    if (pentry.th32ProcessID == pid) {
      CloseHandle(hSnapShot);
      return ScalarString(psw__utf16_to_charsxp(pentry.szExeFile, -1));
    }
    ok = Process32NextW(hSnapShot, &pentry);
  }

  CloseHandle(hSnapShot);
  ps__no_such_process("");
  ps__throw_error();
  return R_NilValue;
}


/*
 * Return process memory information as a list.
 */
SEXP psw__proc_memory_info(SEXP r_pid) {
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
    psw__set_error_from_windows_error(0);
    ps__throw_error();
  }

#if (_WIN32_WINNT >= 0x0501)  // Windows XP with SP2
  private = cnt.PrivateUsage;
#endif

  CloseHandle(hProcess);

  // PROCESS_MEMORY_COUNTERS values are defined as SIZE_T which on 64bits
  // is an (unsigned long long) and on 32bits is an (unsigned int).
  // "_WIN64" is defined if we're running a 64bit interpreter not
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


/*
 * Return process current working directory as a string.
 */
SEXP psw__proc_cwd(SEXP r_pid)  {
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
    psw__set_error_from_windows_error(0);
    return FALSE;
  }

  // Fill in the size of the structure before using it
  te32.dwSize = sizeof(THREADENTRY32);

  if (! Thread32First(hThreadSnap, &te32)) {
    psw__set_error_from_windows_error(0);
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
	psw__set_error_from_windows_error(0);
	CloseHandle(hThread);
	CloseHandle(hThreadSnap);
	return FALSE;
      }
      if (suspend == 1) {
	if (SuspendThread(hThread) == (DWORD) - 1) {
	  psw__set_error_from_windows_error(0);
	  CloseHandle(hThread);
	  CloseHandle(hThreadSnap);
	  return FALSE;
	}
      }
      else {
	if (ResumeThread(hThread) == (DWORD) - 1) {
	  psw__set_error_from_windows_error(0);
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


SEXP psw__proc_suspend(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  int suspend = 1;

  if (! ps__proc_suspend_or_resume(pid, suspend))
    ps__throw_error();

  return R_NilValue;
}


SEXP psw__proc_resume(SEXP r_pid)  {
  long pid = INTEGER(r_pid)[0];
  int suspend = 0;

  if (! ps__proc_suspend_or_resume(pid, suspend))
    ps__throw_error();

  return R_NilValue;
}


/*
  Accept a filename's drive in native  format like "\Device\HarddiskVolume1\"
  and return the corresponding drive letter (e.g. "C:\\").
  If no match is found return an empty string.
*/
SEXP psw__win32_QueryDosDevice(SEXP r_path) {
  WCHAR *lpDevicePath;
  WCHAR d = 'A';

  psw__utf8_to_utf16(CHAR(STRING_ELT(r_path, 0)), &lpDevicePath);

  while (d <= 'Z') {
    WCHAR szDeviceName[3] = {d, ':', '\0' };
    WCHAR szTarget[512] = {0};
    if (QueryDosDeviceW(szDeviceName, szTarget, 511) != 0) {
      if (wcscmp(lpDevicePath, szTarget) == 0) {
	return ScalarString(psw__utf16_to_charsxp(szDeviceName, -1));
      }
    }
    d++;
  }
  return mkString("");
}


/*
 * Return process username as a "DOMAIN//USERNAME" string.
 */
SEXP psw__proc_username(SEXP r_pid) {
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
    psw__set_error_from_windows_error(0);
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
	  psw__set_error_from_windows_error(0);
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
	  psw__set_error_from_windows_error(0);
	  goto error;
	}
      }
    break;
  }

  PROTECT(ret = allocVector(STRSXP, 2));
  SET_STRING_ELT(ret, 0, psw__utf16_to_charsxp(domainName, -1));
  SET_STRING_ELT(ret, 1, psw__utf16_to_charsxp(name, -1));

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


/*
 * Return True if one of the process threads is in a waiting or
 * suspended status.
 */
SEXP psw__proc_is_suspended(SEXP r_pid) {
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

SEXP psw__proc_info(SEXP r_pid) {
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
    SEXP bt = PROTECT(psw__boot_time());
    create_time = REAL(bt)[0];
    UNPROTECT(1);
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


/*
 * Return a {pid:ppid, ...} dict for all running processes.
 */
SEXP psw__ppid_map() {
  SEXP ret;
  HANDLE handle = NULL;
  PROCESSENTRY32 pe = {0};
  size_t num_proc = 0, idx = 0;
  pe.dwSize = sizeof(PROCESSENTRY32);

  handle = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (handle == INVALID_HANDLE_VALUE) {
    psw__set_error_from_windows_error(0);
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


SEXP psw__kill_tree_process(SEXP r_marker, SEXP r_pid) {
  const char *marker = CHAR(STRING_ELT(r_marker, 0));
  long pid = INTEGER(r_pid)[0];
  SEXP env;
  HANDLE hProcess;
  int i, n;
  DWORD err;

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
      psw__set_error_from_windows_error(0);
    }
    ps__throw_error();
  }

  /* Check environment again, to avoid racing */
  PROTECT(env = psw__proc_environ(r_pid));
  n = LENGTH(env);
  for (i = 0; i < n; i++) {
    if (strstr(CHAR(STRING_ELT(env, i)), marker)) {
      if (! TerminateProcess(hProcess, SIGTERM)) {
	err = GetLastError();
	// See: https://github.com/giampaolo/psutil/issues/1099
	if (err != ERROR_ACCESS_DENIED) {
	  CloseHandle(hProcess);
	  psw__set_error_from_windows_error(0);
	  ps__throw_error();
	}
      }
      CloseHandle(hProcess);
      UNPROTECT(1);
      return ScalarLogical(1);
    }
  }

  CloseHandle(hProcess);
  UNPROTECT(1);
  return ScalarLogical(0);
}

SEXP ps__init(SEXP psenv, SEXP constenv) {
  return R_NilValue;
}
