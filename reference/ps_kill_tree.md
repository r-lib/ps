# Mark a process and its (future) child tree

`ps_mark_tree()` generates a random environment variable name and sets
it in the current R process. This environment variable will be (by
default) inherited by all child (and grandchild, etc.) processes, and
will help finding these processes, even if and when they are (no longer)
related to the current R process. (I.e. they are not connected in the
process tree.)

## Usage

``` r
ps_mark_tree()

with_process_cleanup(expr)

ps_find_tree(marker)

ps_kill_tree(marker, sig = signals()$SIGKILL, grace = 200)
```

## Arguments

- expr:

  R expression to evaluate in the new context.

- marker:

  String scalar, the name of the environment variable to use to find the
  marked processes.

- sig:

  The signal to send to the marked processes on Unix. On Windows this
  argument is ignored currently.

- grace:

  Grace period, in milliseconds, used on Unix, if `sig` is `SIGKILL`. If
  it is not zero, then `ps_kill_tree()` first sends a `SIGTERM` signal
  to all processes. If some proccesses do not terminate within `grace`
  milliseconds after the `SIGTERM` signal, `ps_kill_tree()` kills them
  by sending `SIGKILL` signals.

## Value

`ps_mark_tree()` returns the name of the environment variable, which can
be used as the `marker` in `ps_kill_tree()`.

`ps_find_tree()` returns a list of `ps_handle` objects.

`ps_kill_tree()` returns the pids of the killed processes, in a named
integer vector. The names are the file names of the executables, when
available.

`with_process_cleanup()` returns the value of the evaluated expression.

## Details

`ps_find_tree()` finds the processes that set the supplied environment
variable and returns them in a list.

`ps_kill_tree()` finds the processes that set the supplied environment
variable, and kills them (or sends them the specified signal on Unix).

`with_process_cleanup()` evaluates an R expression, and cleans up all
external processes that were started by the R process while evaluating
the expression. This includes child processes of child processes, etc.,
recursively. It returns a list with entries: `result` is the result of
the expression, `visible` is TRUE if the expression should be printed to
the screen, and `process_cleanup` is a named integer vector of the
cleaned pids, names are the process names.

If `expr` throws an error, then so does `with_process_cleanup()`, the
same error. Nevertheless processes are still cleaned up.

## macOS issues

These functions do not work on macOS, unless specific criteria are met.
See [`ps_environ()`](https://ps.r-lib.org/reference/ps_environ.md) for
details.

## Note

Note that `with_process_cleanup()` is problematic if the R process is
multi-threaded and the other threads start subprocesses.
`with_process_cleanup()` cleans up those processes as well, which is
probably not what you want. This is an issue for example in RStudio. Do
not use `with_process_cleanup()`, unless you are sure that the R process
is single-threaded, or the other threads do not start subprocesses. E.g.
using it in package test cases is usually fine, because RStudio runs
these in a separate single-threaded process.

The same holds for manually running `ps_mark_tree()` and then
`ps_find_tree()` or `ps_kill_tree()`.

A safe way to use process cleanup is to use the processx package to
start subprocesses, and set the `cleanup_tree = TRUE` in
[`processx::run()`](http://processx.r-lib.org/reference/run.md) or the
[processx::process](http://processx.r-lib.org/reference/process.md)
constructor.
