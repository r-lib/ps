
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
  return psll_send_signal(p, ScalarInteger(SIGSTOP));
}


SEXP psll_resume(SEXP p) {
  return psll_send_signal(p, ScalarInteger(SIGCONT));
}


SEXP psll_terminate(SEXP p) {
  return psll_send_signal(p, ScalarInteger(SIGTERM));
}


SEXP psll_kill(SEXP p) {
  return psll_send_signal(p, ScalarInteger(SIGKILL));
}
