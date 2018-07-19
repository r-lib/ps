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

#include "common.h"
#include "posix.h"

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

  close(fd);

  return buffer_size - rem_size;

 error:
  if (fd >= 0) close(fd);
  if (*buffer) free(*buffer);
  *buffer = 0;
  return -1;
}

SEXP ps__init(SEXP psenv, SEXP constenv) {

  /* Signals */
  defineVar(install("signals"), ps__define_signals(), constenv);

  /* errno values */
  defineVar(install("errno"), ps__define_errno(), constenv);

  return R_NilValue;
}
