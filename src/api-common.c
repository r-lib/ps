
#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>

#include "ps-internal.h"

SEXP psll_pid(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);
  if (!handle) error("Process pointer cleaned up already");
  return ScalarInteger(handle->pid);
}

SEXP psll_create_time(SEXP p) {
  ps_handle_t *handle = R_ExternalPtrAddr(p);

  if (!handle) error("Process pointer cleaned up already");
  return ScalarReal(handle->create_time);
}
