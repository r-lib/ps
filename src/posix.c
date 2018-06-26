/*
 * Copyright (c) 2009, Giampaolo Rodola'. All rights reserved.
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 *
 * Functions specific to all POSIX compliant platforms.
 */

#include <errno.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>

#include "common.h"

#ifdef PS__SUNOS10
#include "arch/solaris/v10/ifaddrs.h"
#elif PS__AIX
#include "arch/aix/ifaddrs.h"
#else
#include <ifaddrs.h>
#endif

#if defined(PS__LINUX)
#include <netdb.h>
#include <linux/types.h>
#include <linux/if_packet.h>
#elif defined(PS__BSD) || defined(PS__OSX)
#include <netdb.h>
#include <netinet/in.h>
#include <net/if_dl.h>
#include <sys/sockio.h>
#include <net/if_media.h>
#include <net/if.h>
#elif defined(PS__SUNOS)
#include <netdb.h>
#include <sys/sockio.h>
#elif defined(PS__AIX)
#include <netdb.h>
#endif

/*
 * Check if PID exists. Return values:
 * 1: exists
 * 0: does not exist
 * -1: error (Python exception is set)
 */
int ps__pid_exists(long pid) {
  int ret;

  // No negative PID exists, plus -1 is an alias for sending signal
  // too all processes except system ones. Not what we want.
  if (pid < 0)
    return 0;

  // As per "man 2 kill" PID 0 is an alias for sending the signal to
  // every process in the process group of the calling process.
  // Not what we want. Some platforms have PID 0, some do not.
  // We decide that at runtime.
  if (pid == 0) {
#if defined(PS__LINUX) || defined(PS__FREEBSD)
    return 0;
#else
    return 1;
#endif
  }

#if defined(PS__OSX)
  ret = kill((pid_t)pid , 0);
#else
  ret = kill(pid , 0);
#endif

  if (ret == 0)
    return 1;
  else {
    if (errno == ESRCH) {
      // ESRCH == No such process
      return 0;
    }
    else if (errno == EPERM) {
      // EPERM clearly indicates there's a process to deny
      // access to.
      return 1;
    }
    else {
      // According to "man 2 kill" possible error values are
      // (EINVAL, EPERM, ESRCH) therefore we should never get
      // here. If we do let's be explicit in considering this
      // an error.
      ps__set_error_from_errno();
      return -1;
    }
  }
}

SEXP ps__pid_exists2(SEXP r_pid) {
  return ScalarLogical(ps__pid_exists(INTEGER(r_pid)[0]));
}

/*
 * Utility used for those syscalls which do not return a meaningful
 * error that we can translate into an exception which makes sense.
 * As such, we'll have to guess.
 * On UNIX, if errno is set, we return that one (OSError).
 * Else, if PID does not exist we assume the syscall failed because
 * of that so we raise NoSuchProcess.
 * If none of this is true we giveup and raise RuntimeError(msg).
 * This will always set a Python exception and return NULL.
 */
int ps__raise_for_pid(long pid, char *syscall_name) {
  // Set exception to AccessDenied if pid exists else NoSuchProcess.
  if (errno != 0) {
    // Unlikely we get here.
    ps__set_error_from_errno();
    return 0;
  }
  else if (ps__pid_exists(pid) == 0) {
    ps__debug("%s syscall failed and PID %i no longer exists; "
	      "assume NoSuchProcess", syscall_name, pid);
    ps__no_such_process("");
  }
  else {
    ps__set_error("%s syscall failed", syscall_name);
  }
  return 0;
}


/*
 * Given a PID return process priority as a Python integer.
 */
SEXP ps__posix_getpriority(SEXP r_pid) {
  long pid = INTEGER(r_pid)[0];
  int priority;
  errno = 0;

#ifdef PS__OSX
  priority = getpriority(PRIO_PROCESS, (id_t)pid);
#else
  priority = getpriority(PRIO_PROCESS, pid);
#endif
  if (errno != 0) {
    ps__set_error_from_errno();
    ps__throw_error();
  }
  return ScalarInteger(priority);
}


/*
 * Given a PID and a value change process priority.
 */
SEXP ps__posix_setpriority(SEXP r_pid, SEXP r_priority) {
  long pid = INTEGER(r_pid)[0];
  int priority = INTEGER(r_priority)[0];
  int retval;

#ifdef PS__OSX
  retval = setpriority(PRIO_PROCESS, (id_t)pid, priority);
#else
  retval = setpriority(PRIO_PROCESS, pid, priority);
#endif
  if (retval == -1) ps__set_error_from_errno();
  ps__throw_error();
  return R_NilValue;
}

/*
 * Translate a sockaddr struct into a Python string.
 * Return None if address family is not AF_INET* or AF_PACKET.
 */
SEXP ps__convert_ipaddr(struct sockaddr *addr, int family) {
  char buf[NI_MAXHOST];
  int err;
  int addrlen;
  size_t n;
  size_t len;
  const char *data;
  char *ptr;

  if (addr == NULL) {
    return R_NilValue;
  }
  else if (family == AF_INET || family == AF_INET6) {
    if (family == AF_INET)
      addrlen = sizeof(struct sockaddr_in);
    else
      addrlen = sizeof(struct sockaddr_in6);
    err = getnameinfo(addr, addrlen, buf, sizeof(buf), NULL, 0,
		      NI_NUMERICHOST);
    if (err != 0) {
      // XXX we get here on FreeBSD when processing 'lo' / AF_INET6
      // broadcast. Not sure what to do other than returning None.
      // ifconfig does not show anything BTW.
      //PyErr_Format(PyExc_RuntimeError, gai_strerror(err));
      //return NULL;
      return R_NilValue;
    }
    else {
      return mkString(buf);
    }
  }
#ifdef PS__LINUX
  else if (family == AF_PACKET) {
    struct sockaddr_ll *lladdr = (struct sockaddr_ll *)addr;
    len = lladdr->sll_halen;
    data = (const char *)lladdr->sll_addr;
  }
#elif defined(PS__BSD) || defined(PS__OSX)
  else if (addr->sa_family == AF_LINK) {
    // Note: prior to Python 3.4 socket module does not expose
    // AF_LINK so we'll do.
    struct sockaddr_dl *dladdr = (struct sockaddr_dl *)addr;
    len = dladdr->sdl_alen;
    data = LLADDR(dladdr);
  }
#endif
  else {
    // unknown family
    return R_NilValue;
  }

  // AF_PACKET or AF_LINK
  if (len > 0) {
    ptr = buf;
    for (n = 0; n < len; ++n) {
      sprintf(ptr, "%02x:", data[n] & 0xff);
      ptr += 3;
    }
    *--ptr = '\0';
    return mkString(buf);
  }
  else {
    return R_NilValue;
  }
}


/*
 * Return NICs information a-la ifconfig as a list of tuples.
 * TODO: on Solaris we won't get any MAC address.
 */
SEXP ps__net_if_addrs() {
  struct ifaddrs *ifaddr, *ifa;
  int family;

  PROTECT_INDEX ipx;
  SEXP retlist = NULL;
  SEXP tuple = NULL;
  SEXP address = NULL;
  SEXP netmask = NULL;
  SEXP broadcast = NULL;
  SEXP ptp = NULL;

  PROTECT_WITH_INDEX(retlist = allocVector(VECSXP, 0), &ipx);

  if (getifaddrs(&ifaddr) == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  PROTECT_PTR(ifaddr);

  for (ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
    if (!ifa->ifa_addr)
      continue;
    family = ifa->ifa_addr->sa_family;
    address = PROTECT(ps__convert_ipaddr(ifa->ifa_addr, family));
    // If the primary address can't be determined just skip it.
    // I've never seen this happen on Linux but I did on FreeBSD.
    if (isNull(address))
      continue;
    netmask = PROTECT(ps__convert_ipaddr(ifa->ifa_netmask, family));

    if (ifa->ifa_flags & IFF_BROADCAST) {
      broadcast = PROTECT(ps__convert_ipaddr(ifa->ifa_broadaddr, family));
      ptp = R_NilValue;
    }
    else if (ifa->ifa_flags & IFF_POINTOPOINT) {
      ptp = PROTECT(ps__convert_ipaddr(ifa->ifa_dstaddr, family));
      broadcast = R_NilValue;
    }
    else {
      broadcast = R_NilValue;
      ptp = PROTECT(R_NilValue);
    }

    tuple = ps__build_list(
      "siOOOO",
      ifa->ifa_name,
      family,
      address,
      netmask,
      broadcast,
      ptp);
    UNPROTECT(3);
    PROTECT(tuple);

    REPROTECT(retlist = Rf_listAppend(retlist, tuple), ipx);
  }

  UNPROTECT(1);
  return retlist;
}


/*
 * Return NIC MTU. References:
 * http://www.i-scream.org/libstatgrab/
 */
SEXP ps__net_if_mtu(SEXP r_nic_name) {
  const char *nic_name = CHAR(STRING_ELT(r_nic_name, 0));
  int sock = 0;
  int ret;
#ifdef PS__SUNOS10
  struct lifreq lifr;
#else
  struct ifreq ifr;
#endif

  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock == -1)
    goto error;

#ifdef PS__SUNOS10
  strncpy(lifr.lifr_name, nic_name, sizeof(lifr.lifr_name));
  ret = ioctl(sock, SIOCGIFMTU, &lifr);
#else
  strncpy(ifr.ifr_name, nic_name, sizeof(ifr.ifr_name));
  ret = ioctl(sock, SIOCGIFMTU, &ifr);
#endif
  if (ret == -1)
    goto error;
  close(sock);

#ifdef PS__SUNOS10
  return ScalarInteger(lifr.lifr_mtu);
#else
  return ScalarInteger(ifr.ifr_mtu);
#endif

 error:
  if (sock != 0)
    close(sock);
  ps__set_error_from_errno();
  ps__throw_error();
  return R_NilValue;
}


/*
 * Inspect NIC flags, returns a bool indicating whether the NIC is
 * running. References:
 * http://www.i-scream.org/libstatgrab/
 */
SEXP ps__net_if_flags(SEXP r_nic_name) {
  const  char *nic_name = CHAR(STRING_ELT(r_nic_name, 0));
  int sock = 0;
  int ret;
  struct ifreq ifr;

  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock == -1)
    goto error;

  strncpy(ifr.ifr_name, nic_name, sizeof(ifr.ifr_name));
  ret = ioctl(sock, SIOCGIFFLAGS, &ifr);
  if (ret == -1)
    goto error;

  close(sock);
  if ((ifr.ifr_flags & IFF_UP) != 0)
    return ScalarLogical(1);
  else
    return ScalarLogical(0);

 error:
  if (sock != 0)
    close(sock);
  ps__set_error_from_errno();
  ps__throw_error();
  return R_NilValue;
}


/*
 * net_if_stats() OSX/BSD implementation.
 */
#if defined(PS__BSD) || defined(PS__OSX)

int ps__get_nic_speed(int ifm_active) {
  // Determine NIC speed. Taken from:
  // http://www.i-scream.org/libstatgrab/
  // Assuming only ETHER devices
  switch(IFM_TYPE(ifm_active)) {
  case IFM_ETHER:
    switch(IFM_SUBTYPE(ifm_active)) {
#if defined(IFM_HPNA_1) && ((!defined(IFM_10G_LR))		\
			    || (IFM_10G_LR != IFM_HPNA_1))
      // HomePNA 1.0 (1Mb/s)
    case(IFM_HPNA_1):
      return 1;
#endif
      // 10 Mbit
    case(IFM_10_T):  // 10BaseT - RJ45
    case(IFM_10_2):  // 10Base2 - Thinnet
    case(IFM_10_5):  // 10Base5 - AUI
    case(IFM_10_STP):  // 10BaseT over shielded TP
    case(IFM_10_FL):  // 10baseFL - Fiber
      return 10;
      // 100 Mbit
    case(IFM_100_TX):  // 100BaseTX - RJ45
    case(IFM_100_FX):  // 100BaseFX - Fiber
    case(IFM_100_T4):  // 100BaseT4 - 4 pair cat 3
    case(IFM_100_VG):  // 100VG-AnyLAN
    case(IFM_100_T2):  // 100BaseT2
      return 100;
      // 1000 Mbit
    case(IFM_1000_SX):  // 1000BaseSX - multi-mode fiber
    case(IFM_1000_LX):  // 1000baseLX - single-mode fiber
    case(IFM_1000_CX):  // 1000baseCX - 150ohm STP
#if defined(IFM_1000_TX) && !defined(PS__OPENBSD)
      // FreeBSD 4 and others (but NOT OpenBSD) -> #define IFM_1000_T in net/if_media.h
    case(IFM_1000_TX):
#endif
#ifdef IFM_1000_FX
    case(IFM_1000_FX):
#endif
#ifdef IFM_1000_T
    case(IFM_1000_T):
#endif
      return 1000;
#if defined(IFM_10G_SR) || defined(IFM_10G_LR) || defined(IFM_10G_CX4)	\
  || defined(IFM_10G_T)
#ifdef IFM_10G_SR
    case(IFM_10G_SR):
#endif
#ifdef IFM_10G_LR
    case(IFM_10G_LR):
#endif
#ifdef IFM_10G_CX4
    case(IFM_10G_CX4):
#endif
#ifdef IFM_10G_TWINAX
    case(IFM_10G_TWINAX):
#endif
#ifdef IFM_10G_TWINAX_LONG
    case(IFM_10G_TWINAX_LONG):
#endif
#ifdef IFM_10G_T
    case(IFM_10G_T):
#endif
      return 10000;
#endif
#if defined(IFM_2500_SX)
#ifdef IFM_2500_SX
    case(IFM_2500_SX):
#endif
      return 2500;
#endif // any 2.5GBit stuff...
      // We don't know what it is
    default:
      return 0;
    }
    break;

#ifdef IFM_TOKEN
  case IFM_TOKEN:
    switch(IFM_SUBTYPE(ifm_active)) {
    case IFM_TOK_STP4:  // Shielded twisted pair 4m - DB9
    case IFM_TOK_UTP4:  // Unshielded twisted pair 4m - RJ45
      return 4;
    case IFM_TOK_STP16:  // Shielded twisted pair 16m - DB9
    case IFM_TOK_UTP16:  // Unshielded twisted pair 16m - RJ45
      return 16;
#if defined(IFM_TOK_STP100) || defined(IFM_TOK_UTP100)
#ifdef IFM_TOK_STP100
    case IFM_TOK_STP100:  // Shielded twisted pair 100m - DB9
#endif
#ifdef IFM_TOK_UTP100
    case IFM_TOK_UTP100:  // Unshielded twisted pair 100m - RJ45
#endif
      return 100;
#endif
      // We don't know what it is
    default:
      return 0;
    }
    break;
#endif

#ifdef IFM_FDDI
  case IFM_FDDI:
    switch(IFM_SUBTYPE(ifm_active)) {
      // We don't know what it is
    default:
      return 0;
    }
    break;
#endif
  case IFM_IEEE80211:
    switch(IFM_SUBTYPE(ifm_active)) {
    case IFM_IEEE80211_FH1:  // Frequency Hopping 1Mbps
    case IFM_IEEE80211_DS1:  // Direct Sequence 1Mbps
      return 1;
    case IFM_IEEE80211_FH2:  // Frequency Hopping 2Mbps
    case IFM_IEEE80211_DS2:  // Direct Sequence 2Mbps
      return 2;
    case IFM_IEEE80211_DS5:  // Direct Sequence 5Mbps
      return 5;
    case IFM_IEEE80211_DS11:  // Direct Sequence 11Mbps
      return 11;
    case IFM_IEEE80211_DS22:  // Direct Sequence 22Mbps
      return 22;
      // We don't know what it is
    default:
      return 0;
    }
    break;

  default:
    return 0;
  }
}


/*
 * Return stats about a particular network interface.
 * References:
 * http://www.i-scream.org/libstatgrab/
 */
SEXP ps__net_if_duplex_speed(SEXP r_nic_name) {
  const char *nic_name = CHAR(STRING_ELT(r_nic_name, 0));
  int sock = 0;
  int ret;
  int duplex;
  int speed;
  struct ifreq ifr;
  struct ifmediareq ifmed;

  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock == -1)
    goto error;
  strncpy(ifr.ifr_name, nic_name, sizeof(ifr.ifr_name));

  // speed / duplex
  memset(&ifmed, 0, sizeof(struct ifmediareq));
  strlcpy(ifmed.ifm_name, nic_name, sizeof(ifmed.ifm_name));
  ret = ioctl(sock, SIOCGIFMEDIA, (caddr_t)&ifmed);
  if (ret == -1) {
    speed = 0;
    duplex = 0;
  }
  else {
    speed = ps__get_nic_speed(ifmed.ifm_active);
    if ((ifmed.ifm_active | IFM_FDX) == ifmed.ifm_active)
      duplex = 2;
    else if ((ifmed.ifm_active | IFM_HDX) == ifmed.ifm_active)
      duplex = 1;
    else
      duplex = 0;
  }

  close(sock);
  return ps__build_list("ii", duplex, speed);

 error:
  if (sock != 0)
    close(sock);
  ps__set_error_from_errno();
  ps__throw_error();
  return R_NilValue;
}
#endif  // net_if_stats() OSX/BSD implementation

SEXP ps__get_pw_uid(SEXP r_uid) {
  struct passwd *pwd;
  errno = 0;
  pwd = getpwuid(INTEGER(r_uid)[0]);
  if (pwd == NULL) {
    ps__set_error_from_errno();
    ps__throw_error();
  }

  return ps__build_named_list(
    "ssiiss",
    "pw_name",   pwd->pw_name,
    "pw_passwd", pwd->pw_passwd,
    "pw_uid",    pwd->pw_uid,
    "pw_gid",    pwd->pw_gid,
    "pw_dir",    pwd->pw_dir,
    "pw_shell",  pwd->pw_shell);
}

SEXP ps__kill(SEXP r_pid, SEXP r_sig) {
  pid_t pid = INTEGER(r_pid)[0];
  int sig = INTEGER(r_sig)[0];
  int ret = kill(pid, sig);
  if (ret == -1) {
    ps__set_error_from_errno();
    ps__throw_error();
  }
  return R_NilValue;
}

SEXP ps__define_signals() {

  SEXP signalenv = PROTECT(Rf_allocSExp(ENVSXP));

#define PS_ADD_SIGNAL(sig) \
  defineVar(install(#sig), ScalarInteger(sig), signalenv)

  /* macOS */

#ifdef SIGHUP
  PS_ADD_SIGNAL(SIGHUP);
#endif
#ifdef SIGINT
  PS_ADD_SIGNAL(SIGINT);
#endif
#ifdef SIGQUIT
  PS_ADD_SIGNAL(SIGQUIT);
#endif
#ifdef SIGILL
  PS_ADD_SIGNAL(SIGILL);
#endif
#ifdef SIGTRAP
  PS_ADD_SIGNAL(SIGTRAP);
#endif
#ifdef SIGABRT
  PS_ADD_SIGNAL(SIGABRT);
#endif
#ifdef SIGEMT
  PS_ADD_SIGNAL(SIGEMT);
#endif
#ifdef SIGFPE
  PS_ADD_SIGNAL(SIGFPE);
#endif
#ifdef SIGKILL
  PS_ADD_SIGNAL(SIGKILL);
#endif
#ifdef SIGBUS
  PS_ADD_SIGNAL(SIGBUS);
#endif
#ifdef SIGSEGV
  PS_ADD_SIGNAL(SIGSEGV);
#endif
#ifdef SIGSYS
  PS_ADD_SIGNAL(SIGSYS);
#endif
#ifdef SIGPIPE
  PS_ADD_SIGNAL(SIGPIPE);
#endif
#ifdef SIGALRM
  PS_ADD_SIGNAL(SIGALRM);
#endif
#ifdef SIGTERM
  PS_ADD_SIGNAL(SIGTERM);
#endif
#ifdef SIGURG
  PS_ADD_SIGNAL(SIGURG);
#endif
#ifdef SIGSTOP
  PS_ADD_SIGNAL(SIGSTOP);
#endif
#ifdef SIGTSTP
  PS_ADD_SIGNAL(SIGTSTP);
#endif
#ifdef SIGCONT
  PS_ADD_SIGNAL(SIGCONT);
#endif
#ifdef SIGCHLD
  PS_ADD_SIGNAL(SIGCHLD);
#endif
#ifdef SIGTTIN
  PS_ADD_SIGNAL(SIGTTIN);
#endif
#ifdef SIGTTOU
  PS_ADD_SIGNAL(SIGTTOU);
#endif
#ifdef SIGIO
  PS_ADD_SIGNAL(SIGIO);
#endif
#ifdef SIGXCPU
  PS_ADD_SIGNAL(SIGXCPU);
#endif
#ifdef SIGXFSZ
  PS_ADD_SIGNAL(SIGXFSZ);
#endif
#ifdef SIGVTALRM
  PS_ADD_SIGNAL(SIGVTALRM);
#endif
#ifdef SIGPROF
  PS_ADD_SIGNAL(SIGPROF);
#endif
#ifdef SIGWINCH
  PS_ADD_SIGNAL(SIGWINCH);
#endif
#ifdef SIGINFO
  PS_ADD_SIGNAL(SIGINFO);
#endif
#ifdef SIGUSR1
  PS_ADD_SIGNAL(SIGUSR1);
#endif
#ifdef SIGUSR2
  PS_ADD_SIGNAL(SIGUSR2);
#endif

  /* Linux */

#ifdef SIGPOLL
  PS_ADD_SIGNAL(SIGPOLL);
#endif
#ifdef SIGIOT
  PS_ADD_SIGNAL(SIGIOT);
#endif
#ifdef SIGSTKFLT
  PS_ADD_SIGNAL(SIGSTKFLT);
#endif
#ifdef SIGCLD
  PS_ADD_SIGNAL(SIGCLD);
#endif
#ifdef SIGPWR
  PS_ADD_SIGNAL(SIGPWR);
#endif
#ifdef SIGLOST
  PS_ADD_SIGNAL(SIGLOST);
#endif
#ifdef SIGUNUSED
  PS_ADD_SIGNAL(SIGUNUSED);
#endif

#undef PS_ADD_SIGNAL

  UNPROTECT(1);
  return signalenv;
}

SEXP ps__define_errno() {

  SEXP env = PROTECT(Rf_allocSExp(ENVSXP));

#define PS_ADD_ERRNO(err,str) \
  defineVar(install(#err), list2(ScalarInteger(err), mkString(str)), env)

  /* OSX */

#ifdef EPERM
  PS_ADD_ERRNO(EPERM, "Operation not permitted.");
#endif

#ifdef ENOENT
  PS_ADD_ERRNO(ENOENT, "No such file or directory.");
#endif

#ifdef ESRCH
  PS_ADD_ERRNO(ESRCH, "No such process.");
#endif

#ifdef EINTR
  PS_ADD_ERRNO(EINTR, "Interrupted function call.");
#endif

#ifdef EIO
  PS_ADD_ERRNO(EIO, "Input/output error.");
#endif

#ifdef ENXIO
  PS_ADD_ERRNO(ENXIO, "No such device or address.");
#endif

#ifdef E2BIG
  PS_ADD_ERRNO(E2BIG, "Arg list too long.");
#endif

#ifdef ENOEXEC
  PS_ADD_ERRNO(ENOEXEC, "Exec format error.");
#endif

#ifdef EBADF
  PS_ADD_ERRNO(EBADF, "Bad file descriptor.");
#endif

#ifdef ECHILD
  PS_ADD_ERRNO(ECHILD, "No child processes.");
#endif

#ifdef EDEADLK
  PS_ADD_ERRNO(EDEADLK, "Resource deadlock avoided.");
#endif

#ifdef ENOMEM
  PS_ADD_ERRNO(ENOMEM, "Cannot allocate memory.");
#endif

#ifdef EACCES
  PS_ADD_ERRNO(EACCES, "Permission denied.");
#endif

#ifdef EFAULT
  PS_ADD_ERRNO(EFAULT, "Bad address.");
#endif

#ifdef ENOTBLK
  PS_ADD_ERRNO(ENOTBLK, "Not a block device.");
#endif

#ifdef EBUSY
  PS_ADD_ERRNO(EBUSY, "Resource busy.");
#endif

#ifdef EEXIST
  PS_ADD_ERRNO(EEXIST, "File exists.");
#endif

#ifdef EXDEV
  PS_ADD_ERRNO(EXDEV, "Improper link.");
#endif

#ifdef ENODEV
  PS_ADD_ERRNO(ENODEV, "Operation not supported by device.");
#endif

#ifdef ENOTDIR
  PS_ADD_ERRNO(ENOTDIR, "Not a directory.");
#endif

#ifdef EISDIR
  PS_ADD_ERRNO(EISDIR, "Is a directory.");
#endif

#ifdef EINVAL
  PS_ADD_ERRNO(EINVAL, "Invalid argument.");
#endif

#ifdef ENFILE
  PS_ADD_ERRNO(ENFILE, "Too many open files in system.");
#endif

#ifdef EMFILE
  PS_ADD_ERRNO(EMFILE, "Too many open files.");
#endif

#ifdef ENOTTY
  PS_ADD_ERRNO(ENOTTY, "Inappropriate ioctl for device.");
#endif

#ifdef ETXTBSY
  PS_ADD_ERRNO(ETXTBSY, "Text file busy.");
#endif

#ifdef EFBIG
  PS_ADD_ERRNO(EFBIG, "File too large.");
#endif

#ifdef ENOSPC
  PS_ADD_ERRNO(ENOSPC, "Device out of space.");
#endif

#ifdef ESPIPE
  PS_ADD_ERRNO(ESPIPE, "Illegal seek.");
#endif

#ifdef EROFS
  PS_ADD_ERRNO(EROFS, "Read-only file system.");
#endif

#ifdef EMLINK
  PS_ADD_ERRNO(EMLINK, "Too many links.");
#endif

#ifdef EPIPE
  PS_ADD_ERRNO(EPIPE, "Broken pipe.");
#endif

#ifdef EDOM
  PS_ADD_ERRNO(EDOM, "Numerical argument out of domain.");
#endif

#ifdef ERANGE
  PS_ADD_ERRNO(ERANGE, "Numerical result out of range.");
#endif

#ifdef EAGAIN
  PS_ADD_ERRNO(EAGAIN, "Resource temporarily unavailable.");
#endif

#ifdef EINPROGRESS
  PS_ADD_ERRNO(EINPROGRESS, "Operation now in progress.");
#endif

#ifdef EALREADY
  PS_ADD_ERRNO(EALREADY, "Operation already in progress.");
#endif

#ifdef ENOTSOCK
  PS_ADD_ERRNO(ENOTSOCK, "Socket operation on non-socket.");
#endif

#ifdef EDESTADDRREQ
  PS_ADD_ERRNO(EDESTADDRREQ, "Destination address required.");
#endif

#ifdef EMSGSIZE
  PS_ADD_ERRNO(EMSGSIZE, "Message too long.");
#endif

#ifdef EPROTOTYPE
  PS_ADD_ERRNO(EPROTOTYPE, "Protocol wrong type for socket.");
#endif

#ifdef ENOPROTOOPT
  PS_ADD_ERRNO(ENOPROTOOPT, "Protocol not available.");
#endif

#ifdef EPROTONOSUPPORT
  PS_ADD_ERRNO(EPROTONOSUPPORT, "Protocol not supported.");
#endif

#ifdef ESOCKTNOSUPPORT
  PS_ADD_ERRNO(ESOCKTNOSUPPORT, "Socket type not supported.");
#endif

#ifdef ENOTSUP
  PS_ADD_ERRNO(ENOTSUP, "Not supported.");
#endif

#ifdef EPFNOSUPPORT
  PS_ADD_ERRNO(EPFNOSUPPORT, "Protocol family not supported.");
#endif

#ifdef EAFNOSUPPORT
  PS_ADD_ERRNO(EAFNOSUPPORT, "Address family not supported by protocol family.");
#endif

#ifdef EADDRINUSE
  PS_ADD_ERRNO(EADDRINUSE, "Address already in use.");
#endif

#ifdef EADDRNOTAVAIL
  PS_ADD_ERRNO(EADDRNOTAVAIL, "Cannot assign requested address.");
#endif

#ifdef ENETDOWN
  PS_ADD_ERRNO(ENETDOWN, "Network is down.");
#endif

#ifdef ENETUNREACH
  PS_ADD_ERRNO(ENETUNREACH, "Network is unreachable.");
#endif

#ifdef ENETRESET
  PS_ADD_ERRNO(ENETRESET, "Network dropped connection on reset.");
#endif

#ifdef ECONNABORTED
  PS_ADD_ERRNO(ECONNABORTED, "Software caused connection abort.");
#endif

#ifdef ECONNRESET
  PS_ADD_ERRNO(ECONNRESET, "Connection reset by peer.");
#endif

#ifdef ENOBUFS
  PS_ADD_ERRNO(ENOBUFS, "No buffer space available.");
#endif

#ifdef EISCONN
  PS_ADD_ERRNO(EISCONN, "Socket is already connected.");
#endif

#ifdef ENOTCONN
  PS_ADD_ERRNO(ENOTCONN, "Socket is not connected.");
#endif

#ifdef ESHUTDOWN
  PS_ADD_ERRNO(ESHUTDOWN, "Cannot send after socket shutdown.");
#endif

#ifdef ETIMEDOUT
  PS_ADD_ERRNO(ETIMEDOUT, "Operation timed out.");
#endif

#ifdef ECONNREFUSED
  PS_ADD_ERRNO(ECONNREFUSED, "Connection refused.");
#endif

#ifdef ELOOP
  PS_ADD_ERRNO(ELOOP, "Too many levels of symbolic links.");
#endif

#ifdef ENAMETOOLONG
  PS_ADD_ERRNO(ENAMETOOLONG, "File name too long.");
#endif

#ifdef EHOSTDOWN
  PS_ADD_ERRNO(EHOSTDOWN, "Host is down.");
#endif

#ifdef EHOSTUNREACH
  PS_ADD_ERRNO(EHOSTUNREACH, "No route to host.");
#endif

#ifdef ENOTEMPTY
  PS_ADD_ERRNO(ENOTEMPTY, "Directory not empty.");
#endif

#ifdef EPROCLIM
  PS_ADD_ERRNO(EPROCLIM, "Too many processes.");
#endif

#ifdef EUSERS
  PS_ADD_ERRNO(EUSERS, "Too many users.");
#endif

#ifdef EDQUOT
  PS_ADD_ERRNO(EDQUOT, "Disc quota exceeded.");
#endif

#ifdef ESTALE
  PS_ADD_ERRNO(ESTALE, "Stale NFS file handle.");
#endif

#ifdef EBADRPC
  PS_ADD_ERRNO(EBADRPC, "RPC struct is bad.");
#endif

#ifdef ERPCMISMATCH
  PS_ADD_ERRNO(ERPCMISMATCH, "RPC version wrong.");
#endif

#ifdef EPROGUNAVAIL
  PS_ADD_ERRNO(EPROGUNAVAIL, "RPC prog. not avail.");
#endif

#ifdef EPROGMISMATCH
  PS_ADD_ERRNO(EPROGMISMATCH, "Program version wrong.");
#endif

#ifdef EPROCUNAVAIL
  PS_ADD_ERRNO(EPROCUNAVAIL, "Bad procedure for program.");
#endif

#ifdef ENOLCK
  PS_ADD_ERRNO(ENOLCK, "No locks available.");
#endif

#ifdef ENOSYS
  PS_ADD_ERRNO(ENOSYS, "Function not implemented.");
#endif

#ifdef EFTYPE
  PS_ADD_ERRNO(EFTYPE, "Inappropriate file type or format.");
#endif

#ifdef EAUTH
  PS_ADD_ERRNO(EAUTH, "Authentication error.");
#endif

#ifdef ENEEDAUTH
  PS_ADD_ERRNO(ENEEDAUTH, "Need authenticator.");
#endif

#ifdef EPWROFF
  PS_ADD_ERRNO(EPWROFF, "Device power is off.");
#endif

#ifdef EDEVERR
  PS_ADD_ERRNO(EDEVERR, "Device error.");
#endif

#ifdef EOVERFLOW
  PS_ADD_ERRNO(EOVERFLOW, "Value too large to be stored in data type.");
#endif

#ifdef EBADEXEC
  PS_ADD_ERRNO(EBADEXEC, "Bad executable (or shared library).");
#endif

#ifdef EBADARCH
  PS_ADD_ERRNO(EBADARCH, "Bad CPU type in executable.");
#endif

#ifdef ESHLIBVERS
  PS_ADD_ERRNO(ESHLIBVERS, "Shared library version mismatch.");
#endif

#ifdef EBADMACHO
  PS_ADD_ERRNO(EBADMACHO, "Malformed Mach-o file.");
#endif

#ifdef ECANCELED
  PS_ADD_ERRNO(ECANCELED, "Operation canceled.");
#endif

#ifdef EIDRM
  PS_ADD_ERRNO(EIDRM, "Identifier removed.");
#endif

#ifdef ENOMSG
  PS_ADD_ERRNO(ENOMSG, "No message of desired type.");
#endif

#ifdef EILSEQ
  PS_ADD_ERRNO(EILSEQ, "Illegal byte sequence.");
#endif

#ifdef ENOATTR
  PS_ADD_ERRNO(ENOATTR, "Attribute not found.");
#endif

#ifdef EBADMSG
  PS_ADD_ERRNO(EBADMSG, "Bad message.");
#endif

#ifdef EMULTIHOP
  PS_ADD_ERRNO(EMULTIHOP, "Multihop attempted.");
#endif

#ifdef ENODATA
  PS_ADD_ERRNO(ENODATA, "No message available.");
#endif

#ifdef ENOSTR
  PS_ADD_ERRNO(ENOSTR, "Not a STREAM.");
#endif

#ifdef EPROTO
  PS_ADD_ERRNO(EPROTO, "Protocol error.");
#endif

#ifdef ETIME
  PS_ADD_ERRNO(ETIME, "STREAM ioctl() timeout.");
#endif

#ifdef EOPNOTSUPP
  PS_ADD_ERRNO(EOPNOTSUPP, "Operation not supported on socket.");
#endif

  /* Linux */

#ifdef EWOULDBLOCK
  PS_ADD_ERRNO(EWOULDBLOCK, "Resource temporarily unavailable.");
#endif

#ifdef ETOOMANYREFS
  PS_ADD_ERRNO(ETOOMANYREFS, "Too many references: cannot splice.");
#endif

#ifdef EREMOTE
  PS_ADD_ERRNO(EREMOTE, "File is already NFS-mounted");
#endif

#ifdef EBACKGROUND
  PS_ADD_ERRNO(EBACKGROUND, "Caller not in the foreground process group");
#endif

#ifdef EDIED
  PS_ADD_ERRNO(EDIED, "Translator died");
#endif

#ifdef ED
  PS_ADD_ERRNO(ED, "The experienced user will know what is wrong.");
#endif

#ifdef EGREGIOUS
  PS_ADD_ERRNO(EGREGIOUS, "You did *what*?");
#endif

#ifdef EIEIO
  PS_ADD_ERRNO(EIEIO, "Go home and have a glass of warm, dairy-fresh milk.");
#endif

#ifdef EGRATUITOUS
  PS_ADD_ERRNO(EGRATUITOUS, "This error code has no purpose.");
#endif

#ifdef ENOLINK
  PS_ADD_ERRNO(ENOLINK, "Link has been severed.");
#endif

#ifdef ENOSR
  PS_ADD_ERRNO(ENOSR, "Out of streams resources.");
#endif

#ifdef ERESTART
  PS_ADD_ERRNO(ERESTART, "Interrupted system call should be restarted.");
#endif

#ifdef ECHRNG
  PS_ADD_ERRNO(ECHRNG, "Channel number out of range.");
#endif

#ifdef EL2NSYNC
  PS_ADD_ERRNO(EL2NSYNC, "Level 2 not synchronized.");
#endif

#ifdef EL3HLT
  PS_ADD_ERRNO(EL3HLT, "Level 3 halted.");
#endif

#ifdef EL3RST
  PS_ADD_ERRNO(EL3RST, "Level 3 reset.");
#endif

#ifdef ELNRNG
  PS_ADD_ERRNO(ELNRNG, "Link number out of range.");
#endif

#ifdef EUNATCH
  PS_ADD_ERRNO(EUNATCH, "Protocol driver not attached.");
#endif

#ifdef ENOCSI
  PS_ADD_ERRNO(ENOCSI, "No CSI structure available.");
#endif

#ifdef EL2HLT
  PS_ADD_ERRNO(EL2HLT, "Level 2 halted.");
#endif

#ifdef EBADE
  PS_ADD_ERRNO(EBADE, "Invalid exchange.");
#endif

#ifdef EBADR
  PS_ADD_ERRNO(EBADR, "Invalid request descriptor.");
#endif

#ifdef EXFULL
  PS_ADD_ERRNO(EXFULL, "Exchange full.");
#endif

#ifdef ENOANO
  PS_ADD_ERRNO(ENOANO, "No anode.");
#endif

#ifdef EBADRQC
  PS_ADD_ERRNO(EBADRQC, "Invalid request code.");
#endif

#ifdef EBADSLT
  PS_ADD_ERRNO(EBADSLT, "Invalid slot.");
#endif

#ifdef EDEADLOCK
  PS_ADD_ERRNO(EDEADLOCK, "File locking deadlock error.");
#endif

#ifdef EBFONT
  PS_ADD_ERRNO(EBFONT, "Bad font file format.");
#endif

#ifdef ENONET
  PS_ADD_ERRNO(ENONET, "Machine is not on the network.");
#endif

#ifdef ENOPKG
  PS_ADD_ERRNO(ENOPKG, "Package not installed.");
#endif

#ifdef EADV
  PS_ADD_ERRNO(EADV, "Advertise error.");
#endif

#ifdef ESRMNT
  PS_ADD_ERRNO(ESRMNT, "Srmount error.");
#endif

#ifdef ECOMM
  PS_ADD_ERRNO(ECOMM, "Communication error on send.");
#endif

#ifdef EDOTDOT
  PS_ADD_ERRNO(EDOTDOT, "RFS specific error");
#endif

#ifdef ENOTUNIQ
  PS_ADD_ERRNO(ENOTUNIQ, "Name not unique on network.");
#endif

#ifdef EBADFD
  PS_ADD_ERRNO(EBADFD, "File descriptor in bad state.");
#endif

#ifdef EREMCHG
  PS_ADD_ERRNO(EREMCHG, "Remote address changed.");
#endif

#ifdef ELIBACC
  PS_ADD_ERRNO(ELIBACC, "Can not access a needed shared library.");
#endif

#ifdef ELIBBAD
  PS_ADD_ERRNO(ELIBBAD, "Accessing a corrupted shared library.");
#endif

#ifdef ELIBSCN
  PS_ADD_ERRNO(ELIBSCN, ".lib section in a.out corrupted.");
#endif

#ifdef ELIBMAX
  PS_ADD_ERRNO(ELIBMAX, "Attempting to link in too many shared libraries.");
#endif

#ifdef ELIBEXEC
  PS_ADD_ERRNO(ELIBEXEC, "Cannot exec a shared library directly.");
#endif

#ifdef ESTRPIPE
  PS_ADD_ERRNO(ESTRPIPE, "Streams pipe error.");
#endif

#ifdef EUCLEAN
  PS_ADD_ERRNO(EUCLEAN, "Structure needs cleaning.");
#endif

#ifdef ENOTNAM
  PS_ADD_ERRNO(ENOTNAM, "Not a XENIX named type file.");
#endif

#ifdef ENAVAIL
  PS_ADD_ERRNO(ENAVAIL, "No XENIX semaphores available.");
#endif

#ifdef EISNAM
  PS_ADD_ERRNO(EISNAM, "Is a named type file.");
#endif

#ifdef EREMOTEIO
  PS_ADD_ERRNO(EREMOTEIO, "Remote I/O error.");
#endif

#ifdef ENOMEDIUM
  PS_ADD_ERRNO(ENOMEDIUM, "No medium found.");
#endif

#ifdef EMEDIUMTYPE
  PS_ADD_ERRNO(EMEDIUMTYPE, "Wrong medium type.");
#endif

#ifdef ENOKEY
  PS_ADD_ERRNO(ENOKEY, "Required key not available.");
#endif

#ifdef EKEYEXPIRED
  PS_ADD_ERRNO(EKEYEXPIRED, "Key has expired.");
#endif

#ifdef EKEYREVOKED
  PS_ADD_ERRNO(EKEYREVOKED, "Key has been revoked.");
#endif

#ifdef EKEYREJECTED
  PS_ADD_ERRNO(EKEYREJECTED, "Key was rejected by service.");
#endif

#ifdef EOWNERDEAD
  PS_ADD_ERRNO(EOWNERDEAD, "Owner died.");
#endif

#ifdef ENOTRECOVERABLE
  PS_ADD_ERRNO(ENOTRECOVERABLE, "State not recoverable.");
#endif

#ifdef ERFKILL
  PS_ADD_ERRNO(ERFKILL, "Operation not possible due to RF-kill.");
#endif

#ifdef EHWPOISON
  PS_ADD_ERRNO(EHWPOISON, "Memory page has hardware error.");
#endif

#undef PS_ADD_ERRNO

  UNPROTECT(1);
  return env;
}
