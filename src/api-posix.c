
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif

#include <errno.h>

#include "ps-internal.h"

SEXP psll_send_signal(SEXP p, SEXP sig) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  int csig = INTEGER(sig)[0];
  int ret;
  SEXP running;

  if (!handle) error("Process pointer cleaned up already");

  if (handle->pid == 0) {
    error("preventing sending signal to process with PID 0 as it "
	  "would affect every process in the process group of the "
	  "calling process (Sys.getpid()) instead of PID 0");
  }

  running = psll_is_running(p);
  if (!LOGICAL(running)[0]) {
    ps__no_such_process(handle->pid, 0);
    ps__throw_error();
  }

  /* TODO: this is still a race here. We would need to SIGSTOP the
     process first, then check the timestamp, and then send the signal
     (if not SIGSTOP), or send a SIGCONT. */

  ret = kill(handle->pid, csig);
  if (ret == -1) {
    if (errno == ESRCH) {
      ps__no_such_process(handle->pid, 0);
    } else if (errno == EPERM || errno == EACCES) {
      ps__access_denied("");
    } else {
      ps__set_error_from_errno();
    }
    ps__throw_error();
  }

  return R_NilValue;
}


SEXP psll_suspend(SEXP p) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGSTOP));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}


SEXP psll_resume(SEXP p) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGCONT));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}


SEXP psll_terminate(SEXP p) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGTERM));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}


SEXP psll_kill(SEXP p) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGKILL));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}

SEXP psll_interrupt(SEXP p, SEXP ctrlc, SEXP interrupt_path) {
  SEXP res, s;
  PROTECT(s = ScalarInteger(SIGINT));
  PROTECT(res = psll_send_signal(p, s));
  UNPROTECT(2);
  return res;
}
