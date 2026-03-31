# Query or set CPU affinity

`ps_get_cpu_affinity()` queries the [CPU
affinity](https://www.linuxjournal.com/article/6799?page=0,0) of a
process. `ps_set_cpu_affinity()` sets the CPU affinity of a process.

## Usage

``` r
ps_get_cpu_affinity(p = ps_handle())

ps_set_cpu_affinity(p = ps_handle(), affinity)
```

## Arguments

- p:

  Process handle.

- affinity:

  Integer vector of CPU numbers to restrict a process to. CPU numbers
  start with zero, and they have to be smaller than the number of
  (logical) CPUs, see
  [`ps_cpu_count()`](https://ps.r-lib.org/reference/ps_cpu_count.md).

## Value

`ps_get_cpu_affinity()` returns an integer vector of CPU numbers,
starting with zero.

`ps_set_cpu_affinity()` returns `NULL`, invisibly.

## Details

CPU affinity consists in telling the OS to run a process on a limited
set of CPUs only (on Linux cmdline, the `taskset` command is typically
used).

These functions are only supported on Linux and Windows. They error on
macOS.

## Examples

``` r
# current
orig <- ps_get_cpu_affinity()
orig
#> [1] 0 1 2 3

# restrict
ps_set_cpu_affinity(affinity = 0:0)
ps_get_cpu_affinity()
#> [1] 0

# restore
ps_set_cpu_affinity(affinity = orig)
ps_get_cpu_affinity()
#> [1] 0 1 2 3
```
