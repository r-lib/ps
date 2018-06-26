
#ifndef R_PS_EXTRA_H
#define R_PS_EXTRA_H

#define R_USE_C99_IN_CXX 1
#include <Rinternals.h>

#include "common.h"

#ifdef PS__WINDOWS
#include <windows.h>
#endif

SEXP ps__init(SEXP psenv, SEXP constenv);

void PROTECT_PTR(void *ptr);

void *ps__set_error(const char *msg, ...);
void *ps__set_error_from_errno();
SEXP ps__throw_error();

void *ps__access_denied(const char *msg);
void *ps__no_such_process(const char *msg);
void *ps__zombie_process(const char *msg);
void *ps__no_memory(const char *msg);

#ifdef PS__WINDOWS
void *ps__set_error_from_windows_error(long err);
#endif

SEXP ps__str_to_utf8(const char *str);
SEXP ps__str_to_utf8_size(const char *str, size_t size);

#ifdef PS__WINDOWS
SEXP ps__utf16_to_rawsxp(const WCHAR* ws, int size);
SEXP ps__utf16_to_charsxp(const WCHAR* ws, int size);
SEXP ps__utf16_to_strsxp(const WCHAR* ws, int size);
int ps__utf8_to_utf16(const char* s, WCHAR** ws_ptr);
#endif

SEXP ps__build_string(const char *str, ...);
SEXP ps__build_list(const char *template, ...);
SEXP ps__build_named_list(const  char *template, ...);

SEXP ps__os_type();

#ifdef PS__POSIX
SEXP ps__define_signals();
SEXP ps__define_errno();
#endif

#ifdef PS__LINUX
SEXP ps__readlink(SEXP path);
SEXP ps__linux_clk_tck();
SEXP ps__linux_pagesize();
#endif

#endif
