
#include "config.h"

void ps__dummy() {}

#ifndef PS__MACOS
#ifndef PS__WINDOWS
void ps__pids() { }
#endif
#endif

#ifndef PS__POSIX
void psp__pid_exists() { }
void psp__zombie() { }
void psp__waitpid() { }
void psp__pid_exists() { }
void psp__stat_st_rdev() { }
#endif
