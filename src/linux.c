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
 * Return a (ioclass, iodata) Python tuple representing process I/O priority.
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

#if (0)

/*
 * Return disk mounted partitions as a list of tuples including device,
 * mount point and filesystem type
 */
SEXP ps__disk_partitions() {
  FILE *file = NULL;
  struct mntent *entry;
  PyObject *py_dev = NULL;
  PyObject *py_mountp = NULL;
  PyObject *py_tuple = NULL;
  PyObject *py_retlist = PyList_New(0);

  if (py_retlist == NULL)
    return NULL;

  // MOUNTED constant comes from mntent.h and it's == '/etc/mtab'
  Py_BEGIN_ALLOW_THREADS
    file = setmntent(MOUNTED, "r");
  Py_END_ALLOW_THREADS
    if ((file == 0) || (file == NULL)) {
      PyErr_SetFromErrnoWithFilename(PyExc_OSError, MOUNTED);
      goto error;
    }

  while ((entry = getmntent(file))) {
    if (entry == NULL) {
      PyErr_Format(PyExc_RuntimeError, "getmntent() syscall failed");
      goto error;
    }
    py_dev = PyUnicode_DecodeFSDefault(entry->mnt_fsname);
    if (! py_dev)
      goto error;
    py_mountp = PyUnicode_DecodeFSDefault(entry->mnt_dir);
    if (! py_mountp)
      goto error;
    py_tuple = Py_BuildValue("(OOss)",
			     py_dev,             // device
			     py_mountp,          // mount point
			     entry->mnt_type,    // fs type
			     entry->mnt_opts);   // options
    if (! py_tuple)
      goto error;
    if (PyList_Append(py_retlist, py_tuple))
      goto error;
    Py_DECREF(py_dev);
    Py_DECREF(py_mountp);
    Py_DECREF(py_tuple);
  }
  endmntent(file);
  return py_retlist;

 error:
  if (file != NULL)
    endmntent(file);
  Py_XDECREF(py_dev);
  Py_XDECREF(py_mountp);
  Py_XDECREF(py_tuple);
  Py_DECREF(py_retlist);
  return NULL;
}


/*
 * A wrapper around sysinfo(), return system memory usage statistics.
 */
static PyObject *
ps__linux_sysinfo(PyObject *self, PyObject *args) {
  struct sysinfo info;

  if (sysinfo(&info) != 0)
    return PyErr_SetFromErrno(PyExc_OSError);
  // note: boot time might also be determined from here
  return Py_BuildValue(
		       "(kkkkkkI)",
		       info.totalram,  // total
		       info.freeram,  // free
		       info.bufferram, // buffer
		       info.sharedram, // shared
		       info.totalswap, // swap tot
		       info.freeswap,  // swap free
		       info.mem_unit  // multiplier
		       );
}


/*
 * Return process CPU affinity as a Python list
 * The dual implementation exists because of:
 * https://github.com/giampaolo/psutil/issues/536
 */

#ifdef CPU_ALLOC

static PyObject *
ps__proc_cpu_affinity_get(PyObject *self, PyObject *args) {
  int cpu, ncpus, count, cpucount_s;
  long pid;
  size_t setsize;
  cpu_set_t *mask = NULL;
  PyObject *py_list = NULL;

  if (!PyArg_ParseTuple(args, "l", &pid))
    return NULL;
  ncpus = NCPUS_START;
  while (1) {
    setsize = CPU_ALLOC_SIZE(ncpus);
    mask = CPU_ALLOC(ncpus);
    if (mask == NULL)
      return PyErr_NoMemory();
    if (sched_getaffinity(pid, setsize, mask) == 0)
      break;
    CPU_FREE(mask);
    if (errno != EINVAL)
      return PyErr_SetFromErrno(PyExc_OSError);
    if (ncpus > INT_MAX / 2) {
      PyErr_SetString(PyExc_OverflowError, "could not allocate "
		      "a large enough CPU set");
      return NULL;
    }
    ncpus = ncpus * 2;
  }

  py_list = PyList_New(0);
  if (py_list == NULL)
    goto error;

  cpucount_s = CPU_COUNT_S(setsize, mask);
  for (cpu = 0, count = cpucount_s; count; cpu++) {
    if (CPU_ISSET_S(cpu, setsize, mask)) {
#if PY_MAJOR_VERSION >= 3
      PyObject *cpu_num = PyLong_FromLong(cpu);
#else
      PyObject *cpu_num = PyInt_FromLong(cpu);
#endif
      if (cpu_num == NULL)
	goto error;
      if (PyList_Append(py_list, cpu_num)) {
	Py_DECREF(cpu_num);
	goto error;
      }
      Py_DECREF(cpu_num);
      --count;
    }
  }
  CPU_FREE(mask);
  return py_list;

 error:
  if (mask)
    CPU_FREE(mask);
  Py_XDECREF(py_list);
  return NULL;
}
#else


/*
 * Alternative implementation in case CPU_ALLOC is not defined.
 */
static PyObject *
ps__proc_cpu_affinity_get(PyObject *self, PyObject *args) {
  cpu_set_t cpuset;
  unsigned int len = sizeof(cpu_set_t);
  long pid;
  int i;
  PyObject* py_retlist = NULL;
  PyObject *py_cpu_num = NULL;

  if (!PyArg_ParseTuple(args, "l", &pid))
    return NULL;
  CPU_ZERO(&cpuset);
  if (sched_getaffinity(pid, len, &cpuset) < 0)
    return PyErr_SetFromErrno(PyExc_OSError);

  py_retlist = PyList_New(0);
  if (py_retlist == NULL)
    goto error;
  for (i = 0; i < CPU_SETSIZE; ++i) {
    if (CPU_ISSET(i, &cpuset)) {
      py_cpu_num = Py_BuildValue("i", i);
      if (py_cpu_num == NULL)
	goto error;
      if (PyList_Append(py_retlist, py_cpu_num))
	goto error;
      Py_DECREF(py_cpu_num);
    }
  }

  return py_retlist;

 error:
  Py_XDECREF(py_cpu_num);
  Py_XDECREF(py_retlist);
  return NULL;
}
#endif

/*
 * Set process CPU affinity; expects a bitmask
 */
static PyObject *
ps__proc_cpu_affinity_set(PyObject *self, PyObject *args) {
  cpu_set_t cpu_set;
  size_t len;
  long pid;
  int i, seq_len;
  PyObject *py_cpu_set;
  PyObject *py_cpu_seq = NULL;

  if (!PyArg_ParseTuple(args, "lO", &pid, &py_cpu_set))
    return NULL;

  if (!PySequence_Check(py_cpu_set)) {
    PyErr_Format(PyExc_TypeError, "sequence argument expected, got %s",
		 Py_TYPE(py_cpu_set)->tp_name);
    goto error;
  }

  py_cpu_seq = PySequence_Fast(py_cpu_set, "expected a sequence or integer");
  if (!py_cpu_seq)
    goto error;
  seq_len = PySequence_Fast_GET_SIZE(py_cpu_seq);
  CPU_ZERO(&cpu_set);
  for (i = 0; i < seq_len; i++) {
    PyObject *item = PySequence_Fast_GET_ITEM(py_cpu_seq, i);
#if PY_MAJOR_VERSION >= 3
    long value = PyLong_AsLong(item);
#else
    long value = PyInt_AsLong(item);
#endif
    if ((value == -1) || PyErr_Occurred()) {
      if (!PyErr_Occurred())
	PyErr_SetString(PyExc_ValueError, "invalid CPU value");
      goto error;
    }
    CPU_SET(value, &cpu_set);
  }


  len = sizeof(cpu_set);
  if (sched_setaffinity(pid, len, &cpu_set)) {
    PyErr_SetFromErrno(PyExc_OSError);
    goto error;
  }

  Py_DECREF(py_cpu_seq);
  Py_RETURN_NONE;

 error:
  if (py_cpu_seq != NULL)
    Py_DECREF(py_cpu_seq);
  return NULL;
}


/*
 * Return currently connected users as a list of tuples.
 */
static PyObject *
ps__users(PyObject *self, PyObject *args) {
  struct utmp *ut;
  PyObject *py_retlist = PyList_New(0);
  PyObject *py_tuple = NULL;
  PyObject *py_username = NULL;
  PyObject *py_tty = NULL;
  PyObject *py_hostname = NULL;
  PyObject *py_user_proc = NULL;

  if (py_retlist == NULL)
    return NULL;
  setutent();
  while (NULL != (ut = getutent())) {
    py_tuple = NULL;
    py_user_proc = NULL;
    if (ut->ut_type == USER_PROCESS)
      py_user_proc = Py_True;
    else
      py_user_proc = Py_False;
    py_username = PyUnicode_DecodeFSDefault(ut->ut_user);
    if (! py_username)
      goto error;
    py_tty = PyUnicode_DecodeFSDefault(ut->ut_line);
    if (! py_tty)
      goto error;
    py_hostname = PyUnicode_DecodeFSDefault(ut->ut_host);
    if (! py_hostname)
      goto error;
    py_tuple = Py_BuildValue(
			     "(OOOfOi)",
			     py_username,              // username
			     py_tty,                   // tty
			     py_hostname,              // hostname
			     (float)ut->ut_tv.tv_sec,  // tstamp
			     py_user_proc,             // (bool) user process
			     ut->ut_pid                // process id
			     );
    if (! py_tuple)
      goto error;
    if (PyList_Append(py_retlist, py_tuple))
      goto error;
    Py_DECREF(py_username);
    Py_DECREF(py_tty);
    Py_DECREF(py_hostname);
    Py_DECREF(py_tuple);
  }
  endutent();
  return py_retlist;

 error:
  Py_XDECREF(py_username);
  Py_XDECREF(py_tty);
  Py_XDECREF(py_hostname);
  Py_XDECREF(py_tuple);
  Py_DECREF(py_retlist);
  endutent();
  return NULL;
}


/*
 * Return stats about a particular network
 * interface.  References:
 * https://github.com/dpaleino/wicd/blob/master/wicd/backends/be-ioctl.py
 * http://www.i-scream.org/libstatgrab/
 */
static PyObject*
ps__net_if_duplex_speed(PyObject* self, PyObject* args) {
  char *nic_name;
  int sock = 0;
  int ret;
  int duplex;
  int speed;
  struct ifreq ifr;
  struct ethtool_cmd ethcmd;
  PyObject *py_retlist = NULL;

  if (! PyArg_ParseTuple(args, "s", &nic_name))
    return NULL;

  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock == -1)
    goto error;
  strncpy(ifr.ifr_name, nic_name, sizeof(ifr.ifr_name));

  // duplex and speed
  memset(&ethcmd, 0, sizeof ethcmd);
  ethcmd.cmd = ETHTOOL_GSET;
  ifr.ifr_data = (void *)&ethcmd;
  ret = ioctl(sock, SIOCETHTOOL, &ifr);

  if (ret != -1) {
    duplex = ethcmd.duplex;
    speed = ethcmd.speed;
  }
  else {
    if ((errno == EOPNOTSUPP) || (errno == EINVAL)) {
      // EOPNOTSUPP may occur in case of wi-fi cards.
      // For EINVAL see:
      // https://github.com/giampaolo/psutil/issues/797
      //     #issuecomment-202999532
      duplex = DUPLEX_UNKNOWN;
      speed = 0;
    }
    else {
      goto error;
    }
  }

  close(sock);
  py_retlist = Py_BuildValue("[ii]", duplex, speed);
  if (!py_retlist)
    goto error;
  return py_retlist;

 error:
  if (sock != -1)
    close(sock);
  return PyErr_SetFromErrno(PyExc_OSError);
}

#endif

SEXP ps__init(SEXP psenv, SEXP constenv) {
  return R_NilValue;
}
