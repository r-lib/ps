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
