# Get or set the priority of a process

`ps_get_nice()` returns the current priority, `ps_set_nice()` sets a new
priority, `ps_windows_nice_values()` list the possible priority values
on Windows.

## Usage

``` r
ps_windows_nice_values()

ps_get_nice(p = ps_handle())

ps_set_nice(p = ps_handle(), value)
```

## Arguments

- p:

  Process handle.

- value:

  On Windows it must be a string, one of the values of
  `ps_windows_nice_values()`. On Unix it is a priority value that is
  smaller than or equal to 20.

## Value

`ps_windows_nice_values()` return a character vector of possible
priority values on Windows.

`ps_get_nice()` returns a string from `ps_windows_nice_values()` on
Windows. On Unix it returns an integer smaller than or equal to 20.

`ps_set_nice()` return `NULL` invisibly.

## Details

Priority values are different on Windows and Unix.

On Unix, priority is an integer, which is maximum 20. 20 is the lowest
priority.

### Rules:

- On Windows you can only set the priority of the processes the current
  user has `PROCESS_SET_INFORMATION` access rights to. This typically
  means your own processes.

- On Unix you can only set the priority of the your own processes. The
  superuser can set the priority of any process.

- On Unix you cannot set a higher priority, unless you are the
  superuser. (I.e. you cannot set a lower number.)

- On Unix the default priority of a process is zero.
