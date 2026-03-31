# Encode a `ps_handle` as a short string

A convenient format for passing between processes, naming semaphores, or
using as a directory/file name. Will always be 12 alphanumeric
characters, with the first character guarantied to be a letter. Encodes
the pid and creation time for a process.

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
#> <ps::ps_handle> PID=7574, NAME=R, AT=2026-03-31 12:53:41.66
(str <- ps_string(p))
#> [1] "ab8kaaaafPM8"
ps_handle(pid = str)
#> <ps::ps_handle> PID=7574, NAME=R, AT=2026-03-31 12:53:41.66
```
