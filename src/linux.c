/*
 * Copyright (c) 2009, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *
 * Linux-specific functions.
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#include <errno.h>
#include <stdlib.h>
#include <mntent.h>
#include <features.h>
#include <utmp.h>
#include <sched.h>
#include <linux/version.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/sysinfo.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <linux/sockios.h>
#include <linux/if.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>

// see: https://github.com/giampaolo/psutil/issues/659
#ifdef PSUTIL_ETHTOOL_MISSING_TYPES
#include <linux/types.h>
typedef __u64 u64;
typedef __u32 u32;
typedef __u16 u16;
typedef __u8 u8;
#endif
/* Avoid redefinition of struct sysinfo with musl libc */
#define _LINUX_SYSINFO_H
#include <linux/ethtool.h>

/* The minimum number of CPUs allocated in a cpu_set_t */
static const int NCPUS_START = sizeof(unsigned long) * CHAR_BIT;

// Linux >= 2.6.13
#define PSUTIL_HAVE_IOPRIO defined(__NR_ioprio_get) && defined(__NR_ioprio_set)

// Linux >= 2.6.36 (supposedly) and glibc >= 13
#define PSUTIL_HAVE_PRLIMIT				\
  (LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 36)) &&	\
  (__GLIBC__ >= 2 && __GLIBC_MINOR__ >= 13) &&		\
  defined(__NR_prlimit64)

#if PSUTIL_HAVE_PRLIMIT
#define _FILE_OFFSET_BITS 64
#include <time.h>
#include <sys/resource.h>
#endif

#include "common.h"
#include "posix.h"

// May happen on old RedHat versions, see:
// https://github.com/giampaolo/psutil/issues/607
#ifndef DUPLEX_UNKNOWN
#define DUPLEX_UNKNOWN 0xff
#endif


#if PSUTIL_HAVE_IOPRIO
enum {
  IOPRIO_WHO_PROCESS = 1,
};

static inline int
ioprio_get(int which, int who) {
  return syscall(__NR_ioprio_get, which, who);
}

static inline int
ioprio_set(int which, int who, int ioprio) {
  return syscall(__NR_ioprio_set, which, who, ioprio);
}

#define IOPRIO_CLASS_SHIFT 13
#define IOPRIO_PRIO_MASK ((1UL << IOPRIO_CLASS_SHIFT) - 1)

#define IOPRIO_PRIO_CLASS(mask) ((mask) >> IOPRIO_CLASS_SHIFT)
#define IOPRIO_PRIO_DATA(mask) ((mask) & IOPRIO_PRIO_MASK)
#define IOPRIO_PRIO_VALUE(class, data) (((class) << IOPRIO_CLASS_SHIFT) | data)


/*
 * Return a (ioclass, iodata) list representing process I/O priority.
 */
SEXP ps__proc_ioprio_get(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  int ioprio, ioclass, iodata;
  ioprio = ioprio_get(IOPRIO_WHO_PROCESS, pid);
  if (ioprio == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }
  ioclass = IOPRIO_PRIO_CLASS(ioprio);
  iodata = IOPRIO_PRIO_DATA(ioprio);
  return ps__build_list("ii", ioclass, iodata);
}


/*
 * A wrapper around ioprio_set(); sets process I/O priority.
 * ioclass can be either IOPRIO_CLASS_RT, IOPRIO_CLASS_BE, IOPRIO_CLASS_IDLE
 * or 0. iodata goes from 0 to 7 depending on ioclass specified.
 */
SEXP ps__proc_ioprio_set(SEXP r_pid, SEXP r_ioclass, SEXP r_iodata) {
  long pid = INTEGER(r_pid)[0];
  int ioprio = INTEGER(r_ioclass)[0], ioclass = INTEGER(r_iodata)[0], iodata;
  int retval;

  ioprio = IOPRIO_PRIO_VALUE(ioclass, iodata);
  retval = ioprio_set(IOPRIO_WHO_PROCESS, pid, ioprio);
  if (retval == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }
  return R_NilValue;
}
#endif


#if PS__HAVE_PRLIMIT
/*
 * A wrapper around prlimit(2); sets process resource limits.
 * This can be used for both get and set, in which case extra
 * 'soft' and 'hard' args must be provided.
 */
static PyObject *
ps__linux_prlimit(PyObject *self, PyObject *args) {
  long pid;
  int ret, resource;
  struct rlimit old, new;
  struct rlimit *newp = NULL;
  PyObject *py_soft = NULL;
  PyObject *py_hard = NULL;

  if (! PyArg_ParseTuple(args, "li|OO", &pid, &resource, &py_soft, &py_hard))
    return NULL;

  // get
  if (py_soft == NULL && py_hard == NULL) {
    ret = prlimit(pid, resource, NULL, &old);
    if (ret == -1)
      return PyErr_SetFromErrno(PyExc_OSError);
#if defined(PSUTIL_HAVE_LONG_LONG)
    if (sizeof(old.rlim_cur) > sizeof(long)) {
      return Py_BuildValue("LL",
			   (PY_LONG_LONG)old.rlim_cur,
			   (PY_LONG_LONG)old.rlim_max);
    }
#endif
    return Py_BuildValue("ll", (long)old.rlim_cur, (long)old.rlim_max);
  }

  // set
  else {
#if defined(PSUTIL_HAVE_LARGEFILE_SUPPORT)
    new.rlim_cur = PyLong_AsLongLong(py_soft);
    if (new.rlim_cur == (rlim_t) - 1 && PyErr_Occurred())
      return NULL;
    new.rlim_max = PyLong_AsLongLong(py_hard);
    if (new.rlim_max == (rlim_t) - 1 && PyErr_Occurred())
      return NULL;
#else
    new.rlim_cur = PyLong_AsLong(py_soft);
    if (new.rlim_cur == (rlim_t) - 1 && PyErr_Occurred())
      return NULL;
    new.rlim_max = PyLong_AsLong(py_hard);
    if (new.rlim_max == (rlim_t) - 1 && PyErr_Occurred())
      return NULL;
#endif
    newp = &new;
    ret = prlimit(pid, resource, newp, &old);
    if (ret == -1)
      return PyErr_SetFromErrno(PyExc_OSError);
    Py_RETURN_NONE;
  }
}
#endif


int ps__read_file(const char *path, char **buffer, size_t buffer_size) {
  int fd = -1;
  ssize_t ret;
  char *ptr;
  size_t rem_size = buffer_size;

  *buffer = 0;

  fd = open(path, O_RDONLY);
  if (fd == -1) goto error;

  ptr = *buffer = R_alloc(buffer_size, 1);
  if (!*buffer) goto error;

  do {
    if (rem_size == 0) {
      *buffer = S_realloc(*buffer, buffer_size * 2, buffer_size, 1);
      if (!*buffer) goto error;
      ptr = *buffer + buffer_size;
      rem_size = buffer_size;
      buffer_size *= 2;
    }

    ret = read(fd, ptr, rem_size);
    if (ret == -1) goto error;

    ptr += ret;
    rem_size -= ret;
  } while (ret > 0);

  return buffer_size - rem_size;

 error:
  if (fd >= 0) close(fd);
  if (*buffer) free(*buffer);
  *buffer = 0;
  return -1;
}

SEXP psl__parse_stat_file(SEXP r_procfs, SEXP r_pid) {
  const char *procfs = CHAR(STRING_ELT(r_procfs, 0));
  pid_t pid = INTEGER(r_pid)[0];
  char path[512];
  int ret;
  char *buf;
  char *l, *r;

  char state[2] = { 0, 0 };
  int ppid, pgrp, session, tty_nr, tpgid;
  unsigned int flags;
  unsigned long minflt, cminflt, majflt, cmajflt, utime, stime;
  long int cutime, cstime, priority, nice, num_threads, itrealvalue;
  unsigned long long starttime;

  ret = snprintf(path, sizeof(path), "%s/%d/stat", procfs, (int) pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = ps__read_file(path, &buf, /* buffer= */ 2048);
  if (ret == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }
  /* This removed the last character, but that's a \n anyway.
     At least we have a zero terminated string... */
  *(buf + ret) = '\0';

  /* Find the first '(' and last ')', that's the end of the command */
  l = strchr(buf, '(');
  r = strrchr(buf, ')');
  if (!l || !r) {
    ps__set_error("Cannot parse stat file");
    ps__throw_error();
  }

  *r = '\0';

  ret = sscanf(r+2,
    "%c %d %d %d %d %d %u %lu %lu %lu %lu %lu %lu %ld %ld %ld %ld %ld %ld %llu",
    state, &ppid, &pgrp, &session, &tty_nr, &tpgid, &flags, &minflt,
    &cminflt, &majflt, &cmajflt, &utime, &stime, &cutime, &cstime, &priority,
    &nice, &num_threads, &itrealvalue, &starttime);

  if (ret == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  } else if (ret != 20) {
    error("Cannot parse stat file, parsed: %i/20 fields", ret);
  }

  return ps__build_list(
    "ssiiiiiIkkkkkkllllllK",
    /* comm */ l + 1, state, ppid, pgrp, session, tty_nr, tpgid, flags,
    minflt, cminflt, majflt, cmajflt, utime, stime, cutime, cstime,
    priority, nice, num_threads, itrealvalue, starttime);
}

SEXP psl__linux_parse_environ(SEXP r_procfs, SEXP r_pid) {
  const char *procfs = CHAR(STRING_ELT(r_procfs, 0));
  pid_t pid = INTEGER(r_pid)[0];
  char path[512];
  int ret;
  char *buf, *ptr, *end, *prev;
  SEXP result = R_NilValue;
  int nstr = 0;

  ret = snprintf(path, sizeof(path), "%s/%d/environ", procfs, (int) pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = ps__read_file(path, &buf, /* buffer= */ 1024 * 32);
  if (ret == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  /* Count number of vars first, then convert to strings */
  for (ptr = buf, end = buf + ret; ptr < end; ptr++) if (!*ptr) nstr++;

  PROTECT(result = allocVector(STRSXP, nstr));
  for (ptr = prev = buf, nstr = 0; ptr < end; ptr++) {
    if (!*ptr) {
      SET_STRING_ELT(result, nstr++, mkCharLen(prev, ptr - prev));
      prev = ptr + 1;
    }
  }

  UNPROTECT(1);
  return result;
}

#include <string.h>

void *ps__memmem(const void *haystack, size_t n1,
		 const void *needle, size_t n2) {

  const unsigned char *p1 = haystack;
  const unsigned char *p2 = needle;
  const unsigned char *p3 = p1 + n1 - n2 + 1;
  const unsigned char *p;

  if (n2 == 0) return (void*)p1;
  if (n2 > n1) return NULL;

  for (p = p1; (p = memchr(p, *p2, p3 - p)) != NULL; p++) {
    if (!memcmp(p, p2, n2)) return (void*)p;
  }

  return NULL;
}

SEXP psl__linux_match_environ(SEXP r_procfs, SEXP r_marker, SEXP r_pid) {
  const char *procfs = CHAR(STRING_ELT(r_procfs, 0));
  const char *marker = CHAR(STRING_ELT(r_marker, 0));
  pid_t pid = INTEGER(r_pid)[0];
  char path[512];
  int ret;
  char *buf;

  ret = snprintf(path, sizeof(path), "%s/%d/environ", procfs, (int) pid);
  if (ret >= sizeof(path)) {
    ps__set_error("Cannot read proc, path buffer too small");
    ps__throw_error();
  } else if (ret < 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  ret = ps__read_file(path, &buf, /* buffer= */ 1024 * 32);
  if (ret == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  if (ps__memmem(buf, ret, marker, strlen(marker)) == NULL) {
    return ScalarLogical(0);
  } else {
    return ScalarLogical(1);
  }
}

SEXP psl__kill_tree_process(SEXP r_procfs, SEXP r_marker, SEXP r_pid,
			   SEXP r_sig) {

  const char *procfs = CHAR(STRING_ELT(r_procfs, 0));
  const char *marker = CHAR(STRING_ELT(r_marker, 0));
  pid_t pid = INTEGER(r_pid)[0];
  int sig = INTEGER(r_sig)[0];
  char path[512];
  int ret;
  char *buf;
  SEXP match;

  PROTECT(match = psl__linux_match_environ(r_procfs, r_marker, r_pid));

  if (LOGICAL(match)[0]) {
    UNPROTECT(1);
    ret = kill(pid, sig);
    if (ret == -1) {
      ps__set_error_from_errno();
      ps__throw_error();
    }
    return r_pid;

  } else {
    UNPROTECT(1);
    return R_NilValue;
  }
}

SEXP ps__init(SEXP psenv, SEXP constenv) {

  /* Signals */
  defineVar(install("signals"), ps__define_signals(), constenv);

  /* errno values */
  defineVar(install("errno"), ps__define_errno(), constenv);

  return R_NilValue;
}
