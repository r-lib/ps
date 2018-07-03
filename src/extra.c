
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>

#include "extra.h"

#ifdef PS__WINDOWS
#include <windows.h>
#endif

char ps__last_error_string[1024];
SEXP ps__last_error;

/* TODO: these should throw real error objects */

void *ps__set_error_impl(const char *class, int system_errno,
			 const char *msg, ...) {
  va_list args;

  va_start(args, msg);
  vsnprintf(ps__last_error_string,
	    sizeof(ps__last_error_string) - 1, msg, args);
  va_end(args);

  SET_VECTOR_ELT(ps__last_error, 0, mkString(ps__last_error_string));
  if (class) {
    SET_VECTOR_ELT(
      ps__last_error, 1,
      ps__build_string(class, "ps_error", "error", "condition", 0));
  } else {
    SET_VECTOR_ELT(
      ps__last_error, 1,
      ps__build_string("ps_error", "error", "condition", 0));
  }
  SET_VECTOR_ELT(ps__last_error, 2, ScalarInteger(system_errno));
  return NULL;
}

void *ps__set_error(const char *msg, ...) {
  va_list args;

  va_start(args, msg);
  ps__set_error_impl(0, 0, msg, args);
  va_end(args);

  return NULL;
}

void *ps__no_such_process(const char *msg) {
  return ps__set_error_impl("no_such_process", 0,
			    msg && strlen(msg) ? msg : "No such process");
}

void *ps__access_denied(const char *msg) {
  return ps__set_error_impl("access_denied", 0,
			    msg && strlen(msg) ? msg : "Permission denied");
}

void *ps__zombie_process(const char *msg) {
  return ps__set_error_impl("zombie_process", 0,
			    msg && strlen(msg) ? msg : "Process is a zombie");
}

void *ps__no_memory(const char *msg) {
  return ps__set_error_impl("no_memory",
#ifdef PS__WINDOWS
			    ERROR_NOT_ENOUGH_MEMORY,
#else
			    ENOMEM,
#endif
			    msg && strlen(msg) ? msg : "Out of memory");
}

void *ps__set_error_from_errno() {
  if (errno) {
    return ps__set_error_impl("os_error", errno, "%s", strerror(errno));
  } else {
    return ps__set_error_impl(0, errno, "%s", strerror(errno));
  }
}

#ifdef PS__WINDOWS
void *psw__set_error_from_windows_error(long err) {
  /* TODO: get the actual message */
  if (!err) err = GetLastError();
  return ps__set_error_impl("os_error", err, "System error: %i", err);
}
#endif

SEXP ps__throw_error() {
  SEXP stopfun, call, out;

  Rf_setAttrib(ps__last_error, R_ClassSymbol, VECTOR_ELT(ps__last_error, 1));
  PROTECT(stopfun = Rf_findFun(Rf_install("stop"), R_BaseEnv));
  PROTECT(call = Rf_lang2(stopfun, ps__last_error));
  PROTECT(out = Rf_eval(call, R_GlobalEnv));

  UNPROTECT(3);
  return out;
}

void ps__protect_free_finalizer(SEXP ptr) {
  void *vptr = R_ExternalPtrAddr(ptr);
  if (!vptr) return;
  free(vptr);
}

void PROTECT_PTR(void *ptr) {
  SEXP x = PROTECT(R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(x, ps__protect_free_finalizer, 1);
}

SEXP ps__str_to_utf8(const char *str) {
  /* TODO: really convert */
  return mkString(str);
}

SEXP ps__str_to_utf8_size(const char *str, size_t size) {
  /* TODO: really convert */
  return ScalarString(Rf_mkCharLen(str, (int) size));
}

#ifdef PS__WINDOWS

int psw__utf8_to_utf16(const char* s, WCHAR** ws_ptr) {
  int ws_len, r;
  WCHAR* ws;

  ws_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

  if (ws_len <= 0) { return GetLastError(); }

  ws = (WCHAR*) R_alloc(ws_len,  sizeof(WCHAR));
  if (ws == NULL) { return ERROR_OUTOFMEMORY; }

  r = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ ws,
    /* cchWideChar =    */ ws_len);

  if (r != ws_len) {
    error("processx error interpreting UTF8 command or arguments");
  }

  *ws_ptr = ws;
  return 0;
}

SEXP psw__utf16_to_rawsxp(const WCHAR* ws, int size) {
  int s_len, r;
  SEXP s;

  s_len = WideCharToMultiByte(
    /* CodePage =           */ CP_UTF8,
    /* dwFlags =            */ 0,
    /* lpWideCharStr =      */ ws,
    /* cchWideChar =        */ size,
    /* lpMultiByteStr =     */ NULL,
    /* cbMultiByte =        */ 0,
    /* lpDefaultChar =      */ NULL,
    /* lpUsedDefaultChar  = */ NULL);

  if (s_len <= 0) {
    error("error converting wide chars to UTF-8");
  }

  PROTECT(s = allocVector(RAWSXP, s_len));

  r = WideCharToMultiByte(
    /* CodePage =           */ CP_UTF8,
    /* dwFlags =            */ 0,
    /* lpWideCharStr =      */ ws,
    /* cchWideChar =        */ size,
    /* lpMultiByteStr =     */ (char*) RAW(s),
    /* cbMultiByte =        */ s_len,
    /* lpDefaultChar =      */ NULL,
    /* lpUsedDefaultChar  = */ NULL);

  if (r != s_len) {
    error("error converting wide chars to UTF-8");
  }

  UNPROTECT(1);
  return s;
}

SEXP psw__utf16_to_strsxp(const WCHAR* ws, int size) {
  SEXP r, s;
  int r_len, s_len, idx, notr = 0;
  char *ptr, *end, *prev;

  PROTECT(r = psw__utf16_to_rawsxp(ws, size));

  r_len = LENGTH(r);
  ptr = (char*) RAW(r);
  end = ptr + r_len;
  s_len = 0;
  while (ptr < end) {
    if (!*ptr) s_len++;
    ptr++;
  }

  /* If ther is no \0 at the end */
  if (r_len > 0 && *(end - 1) !=  '\0') notr = 1;

  PROTECT(s = allocVector(STRSXP, s_len + notr));

  prev = ptr = (char*) RAW(r);
  idx = 0;
  while (ptr < end) {
    while (ptr < end && *ptr) ptr++;
    SET_STRING_ELT(s, idx++, mkCharLen(prev, ptr - prev));
    prev = ++ptr;
  }

  if (notr) {
    SET_STRING_ELT(s, idx++, mkCharLen(prev, end - prev));
  }

  UNPROTECT(2);
  return s;
}

SEXP psw__utf16_to_charsxp(const WCHAR* ws, int size) {
  SEXP r, s;

  PROTECT(r = psw__utf16_to_rawsxp(ws, size));
  PROTECT(s = mkCharLen((char*) RAW(r), LENGTH(r) - 1));
  UNPROTECT(2);
  return s;
}

#endif

static size_t ps__build_template_length(const char *template) {
  size_t len = 0;
  size_t n = strlen(template);
  size_t i;

  for (i = 0; i < n; i++) {
    len += isalpha(template[i]) != 0;
  }

  return len;
}

SEXP ps__build_string(const char *str, ...) {
  va_list args;
  size_t len = 1;
  SEXP res;
  char *s;

  /* Length 0 character */
  if (!str) return(allocVector(STRSXP, 0));

  /* Count the length first */
  va_start(args, str);
  while (va_arg(args, char*)) len++;
  va_end(args);

  PROTECT(res = allocVector(STRSXP, len));
  SET_STRING_ELT(res, 0, mkChar(str));
  len = 1;
  va_start(args, str);
  while ((s = va_arg(args, char*))) SET_STRING_ELT(res, len++, mkChar(s));
  va_end(args);

  UNPROTECT(1);
  return res;
}

static SEXP ps__build_list_impl(const char *template, int named,
				va_list args) {
  size_t slen = strlen(template);
  size_t len = ps__build_template_length(template);
  SEXP res = PROTECT(allocVector(VECSXP, len));
  SEXP names = named ? PROTECT(allocVector(STRSXP, len)) : R_NilValue;
  int ptr = 0, lptr = 0;

  char *tmp1;
  size_t tmp2;
  char tmp3;

  while (ptr < slen) {
    if (named) {
      SET_STRING_ELT(names, lptr, mkChar(va_arg(args, const char*)));
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

SEXP ps__os_type() {
  SEXP res, names;

  PROTECT(res = allocVector(LGLSXP, 10));
  PROTECT(names = allocVector(STRSXP, 10));

  SET_STRING_ELT(names, 0, mkChar("POSIX"));
  SET_STRING_ELT(names, 1, mkChar("WINDOWS"));
  SET_STRING_ELT(names, 2, mkChar("LINUX"));
  SET_STRING_ELT(names, 3, mkChar("MACOS"));
  SET_STRING_ELT(names, 4, mkChar("FREEBSD"));
  SET_STRING_ELT(names, 5, mkChar("OPENBSD"));
  SET_STRING_ELT(names, 6, mkChar("NETBSD"));
  SET_STRING_ELT(names, 7, mkChar("BSD"));
  SET_STRING_ELT(names, 8, mkChar("SUNOS"));
  SET_STRING_ELT(names, 9, mkChar("AIX"));

  LOGICAL(res)[0] = LOGICAL(res)[1] = LOGICAL(res)[2] = LOGICAL(res)[3] =
    LOGICAL(res)[4] = LOGICAL(res)[5] = LOGICAL(res)[6] = LOGICAL(res)[7] =
    LOGICAL(res)[8] = LOGICAL(res)[9] = 0;

#ifdef PS__POSIX
  LOGICAL(res)[0] = 1;
#endif
#ifdef PS__WINDOWS
  LOGICAL(res)[1] = 1;
#endif
#ifdef PS__LINUX
  LOGICAL(res)[2] = 1;
#endif
#ifdef PS__MACOS
  LOGICAL(res)[3] = 1;
#endif
#ifdef PS__FREEBSD
  LOGICAL(res)[4] = 1;
#endif
#ifdef PS__OPENBSD
  LOGICAL(res)[5] = 1;
#endif
#ifdef PS__NETBSD
  LOGICAL(res)[6] = 1;
#endif
#ifdef PS__BSD
  LOGICAL(res)[7] = 1;
#endif
#ifdef PS__SUNOS
  LOGICAL(res)[8] = 1;
#endif
#ifdef PS__AIX
  LOGICAL(res)[9] = 1;
#endif

  setAttrib(res, R_NamesSymbol, names);
  UNPROTECT(2);
  return res;
}

#ifdef PS__LINUX

/* The lstat() version in readline(2) does not work here,
   because /proc files do not report their size. We just
   need to keep tying with readline(), until the buffer
   size is big enough. */

SEXP psl__readlink(SEXP r_path) {
  const char *path = CHAR(STRING_ELT(r_path, 0));
  char *linkname;
  size_t size = 1024;
  ssize_t r;
  SEXP result;

  linkname = R_alloc(size, 1);

  while (1) {

    r = readlink(path, linkname, size - 1);

    if (r == (ssize_t)-1) {
      ps__set_error_from_errno();
      ps__throw_error();

    } else if (r < (ssize_t)1) {
      errno = ENOENT;
      ps__set_error_from_errno();
      ps__throw_error();

    } else if (r < (ssize_t)(size - 1)) {
      break;
    }

    linkname = S_realloc(linkname, size + 1024, size, 1);
    size += 1024;
  }

  linkname[r] = '\0';

  /* readlink() might return paths containing null bytes ('\x00')
     resulting in "TypeError: must be encoded string without NULL
     bytes, not str" errors when the string is passed to other
     fs-related functions (os.*, open(), ...).
     Apparently everything after '\x00' is garbage (we can have
     ' (deleted)', 'new' and possibly others), see:
     https://github.com/giampaolo/psutil/issues/717

     For us this is not a problem, because mkString uses the string
     up to the first zero byte, anyway.

     The path might still have a ' (deleted)' suffix, we handle
     this in R. */

  PROTECT(result = mkString(linkname));

  UNPROTECT(1);
  return result;
}

SEXP psl__linux_clk_tck() {
  long tck = sysconf(_SC_CLK_TCK);
  return ScalarReal(tck);
}

SEXP psl__linux_pagesize() {
  long ps = sysconf(_SC_PAGE_SIZE);
  return ScalarReal(ps);
}

#endif

static const R_CallMethodDef callMethods[]  = {
  { "ps__init",         (DL_FUNC) ps__init,         2 },
  { "ps__os_type",      (DL_FUNC) ps__os_type,      0 },

  /* POSIX */
  { "psp__pid_exists",   (DL_FUNC) psp__pid_exists2,  1 },
  { "psp__get_pw_uid",   (DL_FUNC) psp__get_pw_uid,   1 },
  { "psp__kill",         (DL_FUNC) psp__kill,         2 },
  { "psp__stat_st_rdev", (DL_FUNC) psp__stat_st_rdev, 1 },

  /* MACOS */
  { "psm__pids",         (DL_FUNC) psm__pids,         0 },
  { "psm__proc_exe",     (DL_FUNC) psm__proc_exe,     1 },
  { "psm__proc_cmdline", (DL_FUNC) psm__proc_cmdline, 1 },
  { "psm__proc_environ", (DL_FUNC) psm__proc_environ, 1 },
  { "psm__proc_cwd",     (DL_FUNC) psm__proc_cwd,     1 },
  { "psm__proc_kinfo_oneshot",
    (DL_FUNC) psm__proc_kinfo_oneshot, 1 },
  { "psm__proc_pidtaskinfo_oneshot",
    (DL_FUNC) psm__proc_pidtaskinfo_oneshot, 1 },

  /* LINUX */
  { "psl__readlink",            (DL_FUNC) psl__readlink,            1 },
  { "psl__linux_clk_tck",       (DL_FUNC) psl__linux_clk_tck,       0 },
  { "psl__linux_pagesize",      (DL_FUNC) psl__linux_pagesize,      0 },
  { "psl__parse_stat_file",     (DL_FUNC) psl__parse_stat_file,     2 },
  { "psl__linux_parse_environ", (DL_FUNC) psl__linux_parse_environ, 2 },
  { "psl__linux_match_environ", (DL_FUNC) psl__linux_match_environ, 3 },
  { "psl__kill_tree_process",   (DL_FUNC) psl__kill_tree_process,   4 },

  /* WINDOWS */
  { "psw__pids",             (DL_FUNC) psw__pids,             0 },
  { "psw__ppid_map",         (DL_FUNC) psw__ppid_map,         0 },
  { "psw__pid_exists",       (DL_FUNC) psw__pid_exists,       1 },
  { "psw__boot_time",        (DL_FUNC) psw__boot_time,        0 },
  { "psw__proc_name",        (DL_FUNC) psw__proc_name,        1 },
  { "psw__proc_exe",         (DL_FUNC) psw__proc_exe,         1 },
  { "psw__proc_cmdline",     (DL_FUNC) psw__proc_cmdline,     1 },
  { "psw__proc_environ",     (DL_FUNC) psw__proc_environ,     1 },
  { "psw__proc_cwd",         (DL_FUNC) psw__proc_cwd,         1 },
  { "psw__proc_username",    (DL_FUNC) psw__proc_username,    1 },
  { "psw__proc_info",        (DL_FUNC) psw__proc_info,        1 },
  { "psw__proc_memory_info", (DL_FUNC) psw__proc_memory_info, 1 },
  { "psw__proc_cpu_times",   (DL_FUNC) psw__proc_cpu_times,   1 },
  { "psw__proc_create_time", (DL_FUNC) psw__proc_create_time, 1 },
  { "psw__proc_is_suspended",(DL_FUNC) psw__proc_is_suspended,1 },
  { "psw__proc_suspend",     (DL_FUNC) psw__proc_suspend,     1 },
  { "psw__proc_resume",      (DL_FUNC) psw__proc_resume,      1 },
  { "psw__proc_kill",        (DL_FUNC) psw__proc_kill,        1 },
  { "psw__win32_QueryDosDevice",
    (DL_FUNC) psw__win32_QueryDosDevice, 1 },
  { "psw__kill_tree_process", (DL_FUNC) psw__kill_tree_process, 2 },

  { NULL, NULL, 0 }
};

/*
 * Called on module import on all platforms.
 */
void R_init_ps(DllInfo *dll) {
  if (getenv("R_PS_DEBUG") != NULL) PS__DEBUG = 1;
  if (getenv("R_PS_TESTING") != NULL) PS__TESTING = 1;

  PROTECT(ps__last_error = ps__build_named_list(
    "ssi",
    "message", "Unknown error",
    "class", "fs_error",
    "errno", 0
  ));

  R_PreserveObject(ps__last_error);
  UNPROTECT(1);

  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
