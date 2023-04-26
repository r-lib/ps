#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <poll.h>
#include <Rinternals.h>
#include "posix.h"
#include "ps-internal.h"

// Should that be a global option shared by processx and ps?
#define PS_INTERRUPT_INTERVAL 200

static struct sigaction old_sig_handler;
static int kill_parallel_pipe = -1;

static void kill_parallel_sigchld_callback(int sig, siginfo_t *info, void *ctx);
static void kill_parallel_setup_sigchld(struct sigaction *prev);
static void kill_parallel_remove_sigchld(struct sigaction *prev);
static void block_sigchld_save(sigset_t *old);
static void procmask_set(sigset_t *set);
static void unblock_sigchld(void);

SEXP ps__kill_parallel(SEXP ps, SEXP ffi_grace) {
  SEXP cont = PROTECT(R_MakeUnwindCont());
  SEXP err = NULL;

  sigset_t old_mask;
  struct sigaction old_action;

  block_sigchld_save(&old_mask);
  kill_parallel_setup_sigchld(&old_action);

  int ps_n = length(ps);

  SEXP killed_shelter = PROTECT(allocVector(LGLSXP, ps_n));
  int* killed = LOGICAL(killed_shelter);

  int killed_n = 0;
  memset(killed, 0, sizeof(int) * ps_n);

  int fds[2] = { -1 };
  if (pipe(fds)) {
    ps__set_error("Can't create pipe for parallel kill.");
    err = ps__last_error;
    goto cleanup;
  }
  kill_parallel_pipe = fds[1];

  ps__nonblock_fcntl(fds[0], 1);
  ps__nonblock_fcntl(fds[1], 1);

  // TODO: Check `create_time`
  for (int i = 0; i < ps_n; ++i) {
    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(ps, i));
    kill(handle->pid, SIGTERM);
  }

  struct pollfd fd = {
    .fd = fds[0],
    .events = POLLIN,
    .revents = 0
  };
  unblock_sigchld();

  int timeleft = ceil(REAL(ffi_grace)[0] * 1000);
  if (timeleft < 0) {
    ps__set_error("`grace` can't be negative.");
    err = ps__last_error;
    goto cleanup;
  }

  int ready = 0;

  while (timeleft > PS_INTERRUPT_INTERVAL) {
    do {
      ready = poll(&fd, 1, PS_INTERRUPT_INTERVAL);
    } while (ready == -1 && errno == EINTR);

    // Rough ETA computation that doesn't take into account time spent
    // until woken up by a SIGCHLD
    if (ready == 0) {
      timeleft -= PS_INTERRUPT_INTERVAL;
    }

    char buf[1];
    int res = 0;

    do {
      res = read(fds[0], buf, 1);
    } while (res == 1 || (res == -1 && errno == EINTR));

    // Should we restore our sigchld handler in case it was overridden
    // by R code?
    if ((err = r_check_for_user_interrupt(cont))) {
      goto cleanup;
    }

    // TODO: Check `create_time`
    for (int i = 0; i < ps_n; ++i) {
      if (killed[i]) {
        continue;
      }

      ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(ps, i));

      if (kill(handle->pid, 0) != 0) {
        killed_n += 1;
        killed[i] = 1;
      }
    }

    if (killed_n == ps_n) {
      goto cleanup;
    }
  }

  if (killed_n == ps_n) {
    goto cleanup;
  }

  /* Maybe we are not done, and there is a little left from the timeout */
  if (ready == 0 && timeleft >= 0) {
    do {
      ready = poll(&fd, 1, timeleft);
    } while (ready == -1 && errno == EINTR);
  }

  // If some processes are still running after the grace period,
  // resort to SIGKILL
  // TODO: Check `create_time`
  for (int i = 0; i < ps_n; ++i) {
    if (killed[i]) {
      continue;
    }

    ps_handle_t *handle = R_ExternalPtrAddr(VECTOR_ELT(ps, i));
    kill(handle->pid, SIGKILL);
  }

 cleanup:
  kill_parallel_pipe = -1;

  if (fds[0] >= 0) close(fds[0]);
  if (fds[1] >= 0) close(fds[1]);

  kill_parallel_remove_sigchld(&old_action);
  procmask_set(&old_mask);

  if (err) {
    r_unwind(cont);
  }

  UNPROTECT(2);
  return R_NilValue;
}


// SIGCHLD -------------------------------------------------------------

static
void kill_parallel_sigchld_callback(int sig, siginfo_t *info, void *ctx) {
  int saved_errno = errno;

  /* This might be called on another thread, if the main thread blocks
     the signal temporarily, so we forward it to the main thread.
     It is OK to call pthread_self() and pthread_kill() in the signal
     handler, according to POSIX.1-2008 TC1.
     https://man7.org/linux/man-pages/man7/signal-safety.7.html */
  if (pthread_self() != ps__main_thread) {
    pthread_kill(ps__main_thread, SIGCHLD);
    errno = saved_errno;
    return;
  }

  int res = 0;
  do {
    res = write(kill_parallel_pipe, "1", 1);
  } while (res == -1 && errno == EINTR);

  // Notify next handler
  if (old_sig_handler.sa_handler != SIG_DFL &&
      old_sig_handler.sa_handler != SIG_IGN &&
      old_sig_handler.sa_handler != NULL) {
    if (old_sig_handler.sa_flags | SA_SIGINFO) {
      old_sig_handler.sa_sigaction(sig, info, NULL);
    } else {
      old_sig_handler.sa_handler(sig);
    }
  }

  errno = saved_errno;
}

static
void kill_parallel_setup_sigchld(struct sigaction *prev) {
  if (!prev) {
    PS__STOP_INTERNAL("Must provide memory to save current handler.");
  }

  sigset_t old_mask;
  block_sigchld_save(&old_mask);

  memcpy(prev, &old_sig_handler, sizeof(old_sig_handler));

  struct sigaction action = {{ 0 }};
  action.sa_sigaction = &kill_parallel_sigchld_callback;
  action.sa_flags = SA_SIGINFO | SA_RESTART | SA_NOCLDSTOP;
  sigaction(SIGCHLD, &action, &old_sig_handler);

  procmask_set(&old_mask);
}

static
void kill_parallel_remove_sigchld(struct sigaction *prev) {
  if (!prev) {
    PS__STOP_INTERNAL("Must provide memory to save current handler.");
  }

  sigset_t old_mask;
  block_sigchld_save(&old_mask);

  sigaction(SIGCHLD, &old_sig_handler, NULL);
  memcpy(&old_sig_handler, prev, sizeof(*prev));

  procmask_set(&old_mask);
}


static
void block_sigchld_save(sigset_t *old) {
  sigset_t new;
  sigemptyset(&new);
  sigaddset(&new, SIGCHLD);

  if (sigprocmask(SIG_BLOCK, &new, old) == -1) {
    PS__STOP_INTERNAL("Can't set up sigprocmask");
  }
}

static
void procmask_set(sigset_t *set) {
  if (sigprocmask(SIG_SETMASK, set, NULL) == -1) {
    PS__STOP_INTERNAL("Can't set up sigprocmask");
  }
}

static
void unblock_sigchld(void) {
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGCHLD);

  if (sigprocmask(SIG_UNBLOCK, &mask, NULL) == -1) {
    PS__STOP_INTERNAL("Can't set up sigprocmask");
  }
}
