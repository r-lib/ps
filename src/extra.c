
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>

#include "extra.h"

void ps__protect_free_finalizer(SEXP ptr) {
  void *vptr = R_ExternalPtrAddr(ptr);
  if (!vptr) return;
  free(vptr);
}

void ps__set_error_from_errno() {
  ps__set_error("System error: %s", strerror(errno));
}

void ps__clear_error() {
  ps__set_error("");
}

void ps__throw_error() {
  const char *last  = ps__get_error();
  error(last && last[0] ? last : "Unknown error");
}

SEXP ps__str_to_utf8(const char *str) {
  /* TODO: really convert */
  return mkString(str);
}

SEXP ps__str_to_utf8_size(const char *str, size_t size) {
  /* TODO: really convert */
  return ScalarString(Rf_mkCharLen(str, (int) size));
}

static size_t ps__build_template_length(const char *template) {
  size_t len = 0;
  size_t n = strlen(template);
  size_t i;

  for (i = 0; i < n; i++) {
    len += isalpha(template[i]);
  }

  return len;
}

static SEXP ps__build_list_impl(const char *template, int named,
				va_list args) {
  size_t slen = strlen(template);
  size_t len = ps__build_template_length(template);
  SEXP res = PROTECT(allocVector(LISTSXP, len));
  SEXP names = named ? PROTECT(allocVector(STRSXP, len)) : R_NilValue;
  int ptr = 0, lptr = 0;

  char *tmp1;
  size_t tmp2;
  char tmp3;

  while (ptr < slen) {
    if (named) {
      SET_STRING_ELT(names, lptr, mkString(va_arg(args, const char*)));
    }

    switch(template[ptr]) {

    case 's':
    case 'z':
    case 'U':
      tmp1 = va_arg(args, char*);
      SET_VECTOR_ELT(res, lptr, tmp1 ? mkString(tmp1) : R_NilValue);
      break;

    case 'y':
      tmp1 = va_arg(args, char*);
      tmp2 = strlen(tmp1);
      SET_VECTOR_ELT(res, lptr, allocVector(RAWSXP, tmp2));
      memcpy(RAW(VECTOR_ELT(res, lptr)), tmp1, tmp2);
      break;
    case 'u':
      error("'u' is not implemented yet");
      break;

    case 'i':
    case 'b':
    case 'h':
    case 'B':
    case 'H':
      SET_VECTOR_ELT(res, lptr, ScalarInteger(va_arg(args, int)));
      SET_VECTOR_ELT(res, lptr, ScalarInteger(va_arg(args, int)));
      break;

    case 'l':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, long int)));
      break;

    case 'I':
      SET_VECTOR_ELT(res, lptr, ScalarInteger(va_arg(args, unsigned int)));
      break;

    case 'k':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, unsigned long)));
      break;

    case 'L':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, long long)));
      break;

    case 'K':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, unsigned long long)));
      break;

    case 'n':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, size_t)));
      break;

    case 'c':
      tmp3 = (char) va_arg(args, int);
      SET_VECTOR_ELT(res, lptr, ScalarRaw(tmp3));
      break;

    case 'C':
      tmp3 = (char) va_arg(args, int);
      SET_VECTOR_ELT(res, lptr, ScalarString(mkCharLen(&tmp3, 1)));
      break;

    case 'd':
    case 'f':
      SET_VECTOR_ELT(res, lptr, ScalarReal(va_arg(args, double)));
      break;

    case 'D':
      error("'D' is not implemented yet");
      break;

    case 'S':
    case 'N':
    case 'O':
      SET_VECTOR_ELT(res, lptr, (SEXP) va_arg(args, void*));
      break;

    default:
      error("Unknown conversion key: `%c`", template[ptr]);
    }
    ptr++;
    lptr++;
  }

  if (named) {
    setAttrib(res, R_NamesSymbol, names);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return res;
}

SEXP ps__build_list(const char *template, ...) {
  va_list args;
  SEXP res;
  va_start(args, template);
  res = PROTECT(ps__build_list_impl(template, 0, args));
  va_end(args);
  UNPROTECT(1);
  return res;
}

SEXP ps__build_named_list(const char *template, ...) {
  va_list args;
  SEXP res;
  va_start(args, template);
  res = PROTECT(ps__build_list_impl(template, 1, args));
  va_end(args);
  UNPROTECT(1);
  return res;
}

static const R_CallMethodDef callMethods[]  = {
  { "ps__pids",       (DL_FUNC) ps__pids,       0 },
  { "ps__pid_exists", (DL_FUNC) ps__pid_exists2, 1 },

  { NULL, NULL, 0 }
};

/*
 * Called on module import on all platforms.
 */
void R_init_ps(DllInfo *dll) {
  if (getenv("R_PS_DEBUG") != NULL) PS__DEBUG = 1;
  if (getenv("R_PS_TESTING") != NULL) PS__TESTING = 1;

  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
