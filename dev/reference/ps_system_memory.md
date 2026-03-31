# Statistics about system memory usage

Statistics about system memory usage

## Usage

``` r
ps_system_memory()
```

## Value

Named list. All numbers are in bytes:

- `total`: total physical memory (exclusive swap).

- `avail` the memory that can be given instantly to processes without
  the system going into swap. This is calculated by summing different
  memory values depending on the platform and it is supposed to be used
  to monitor actual memory usage in a cross platform fashion.

- `percent`: Percentage of memory that is taken.

- `used`: memory used, calculated differently depending on the platform
  and designed for informational purposes only. `total` - `free` does
  not necessarily match `used`.

- `free`: memory not being used at all (zeroed) that is readily
  available; note that this doesn’t reflect the actual memory available
  (use `available` instead). `total` - `used` does not necessarily match
  `free`.

- `active`: (Unix only) memory currently in use or very recently used,
  and so it is in RAM.

- `inactive`: (Unix only) memory that is marked as not used.

- `wired`: (macOS only) memory that is marked to always stay in RAM. It
  is never moved to disk.

- `buffers`: (Linux only) cache for things like file system metadata.

- `cached`: (Linux only) cache for various things.

- `shared`: (Linux only) memory that may be simultaneously accessed by
  multiple processes.

- `slab`: (Linux only) in-kernel data structures cache.

## See also

Other memory functions:
[`ps_system_swap()`](https://ps.r-lib.org/dev/reference/ps_system_swap.md)

## Examples

``` r
ps_system_memory()
#> $total
#> [1] 16766443520
#> 
#> $avail
#> [1] 15234891776
#> 
#> $percent
#> [1] 9.134625
#> 
#> $used
#> [1] 1117073408
#> 
#> $free
#> [1] 10715582464
#> 
#> $active
#> [1] 1099120640
#> 
#> $inactive
#> [1] 4116819968
#> 
#> $buffers
#> [1] 127270912
#> 
#> $cached
#> [1] 4806516736
#> 
#> $shared
#> [1] 51273728
#> 
#> $slab
#> [1] 311734272
#> 
```
