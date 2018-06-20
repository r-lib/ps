
#ifndef R_PS_EXTRA_H
#define R_PS_EXTRA_H

#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>

#include "common.h"

void ps__protect_free_finalizer(SEXP ptr);

#define PROTECT_FREE(x) do {						  \
  SEXP x ## _ptr = PROTECT(R_MakeExternalPtr(x, R_NilValue, R_NilValue)); \
  R_RegisterCFinalizerEx(x ## _ptr, ps__protect_free_finalizer, 1);	  \
  } while (0)

void ps__set_error_from_errno();
void ps__clear_error();
void ps__throw_error();

SEXP ps__str_to_utf8(const char *str);
SEXP ps__str_to_utf8_size(const char *str, size_t size);

SEXP ps__build_list(const char *template, ...);
SEXP ps__build_named_list(const  char *template, ...);

SEXP ps__os_type();

#ifdef PS__LINUX
SEXP ps__readlink(SEXP path);
SEXP ps__linux_clk_tck();
SEXP ps__linux_pagesize();
#endif

#endif
