# testthat reporter that checks if child processes are cleaned up in tests

`CleanupReporter` takes an existing testthat `Reporter` object, and
wraps it, so it checks for leftover child processes, at the specified
place, see the `proc_unit` argument below.

## Usage

``` r
CleanupReporter(reporter = testthat::ProgressReporter)
```

## Arguments

- reporter:

  A testthat reporter to wrap into a new `CleanupReporter` class.

## Value

New reporter class that behaves exactly like `reporter`, but it checks
for, and optionally cleans up child processes, at the specified
granularity.

## Details

Child processes can be reported via a failed expectation, cleaned up
silently, or cleaned up and reported (the default).

If a `test_that()` block has an error, `CLeanupReporter` does not emit
any expectations at the end of that block. The error will lead to a test
failure anyway. It will still perform the cleanup, if requested,
however.

The constructor of the `CleanupReporter` class has options:

- `file`: the output file, if any, this is passed to `reporter`.

- `proc_unit`: when to perform the child process check and cleanup.
  Possible values:

  - `"test"`: at the end of each
    [`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
    block (the default),

  - `"testsuite"`: at the end of the test suite.

- `proc_cleanup`: Logical scalar, whether to kill the leftover
  processes, `TRUE` by default.

- `proc_fail`: Whether to create an expectation, that fails if there are
  any processes alive, `TRUE` by default.

- `proc_timeout`: How long to wait for the processes to quit. This is
  sometimes needed, because even if some kill signals were sent to child
  processes, it might take a short time for these to take effect. It
  defaults to one second.

- `rconn_unit`: When to perform the R connection cleanup. Possible
  values are `"test"` and `"testsuite"`, like for `proc_unit`.

- `rconn_cleanup`: Logical scalar, whether to clean up leftover R
  connections. `TRUE` by default.

- `rconn_fail`: Whether to fail for leftover R connections. `TRUE` by
  default.

- `file_unit`: When to check for open files. Possible values are
  `"test"` and `"testsuite"`, like for `proc_unit`.

- `file_fail`: Whether to fail for leftover open files. `TRUE` by
  default.

- `conn_unit`: When to check for open network connections. Possible
  values are `"test"` and `"testsuite"`, like for `proc_unit`.

- `conn_fail`: Whether to fail for leftover network connections. `TRUE`
  by default.

## Note

Some IDEs, like RStudio, start child processes frequently, and sometimes
crash when these are killed, only use this reporter in a terminal
session. In particular, you can always use it in the idiomatic
`testthat.R` file, that calls `test_check()` during `R CMD check`.

## Examples

This is how to use this reporter in `testthat.R`:

    library(testthat)
    library(mypackage)

    if  (ps::ps_is_supported()) {
      reporter <- ps::CleanupReporter(testthat::ProgressReporter)$new(
        proc_unit = "test", proc_cleanup = TRUE)
    } else {
      ## ps does not support this platform
      reporter <- "progress"
    }

    test_check("mypackage", reporter = reporter)
