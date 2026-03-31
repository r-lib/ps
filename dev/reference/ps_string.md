# Encode a `ps_handle` as a short string

A convenient format for passing between processes, naming semaphores, or
using as a directory/file name. Will always be 14 alphanumeric
characters, with the first and last characters guarantied to be letters.
Encodes the pid and creation time for a process.

## Usage

``` r
ps_string(p = ps_handle())
```

## Arguments

- p:

  Process handle.

## Value

A process string (scalar character), that can be passed to
[`ps_handle()`](https://ps.r-lib.org/dev/reference/ps_handle.md) in
place of a pid.

## Examples

``` r
(p <- ps_handle())
#> <ps::ps_handle> PID=7631, NAME=R, AT=2026-03-31 11:51:58.99
(str <- ps_string(p))
#> [1] "acQNb6hHNGejHU"
ps_handle(pid = str)
#> <ps::ps_handle> PID=7631, NAME=R, AT=2026-03-31 11:51:58.99
```
