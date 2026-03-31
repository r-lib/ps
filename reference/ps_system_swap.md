# System swap memory statistics

System swap memory statistics

## Usage

``` r
ps_system_swap()
```

## Value

Named list. All numbers are in bytes:

- `total`: total swap memory.

- `used`: used swap memory.

- `free`: free swap memory.

- `percent`: the percentage usage.

- `sin`: the number of bytes the system has swapped in from disk
  (cumulative). This is `NA` on Windows.

- `sout`: the number of bytes the system has swapped out from disk
  (cumulative). This is `NA` on Windows.

## See also

Other memory functions:
[`ps_system_memory()`](https://ps.r-lib.org/reference/ps_system_memory.md)

## Examples

``` r
ps_system_swap()
#> $total
#> [1] 3221221376
#> 
#> $used
#> [1] 0
#> 
#> $free
#> [1] 3221221376
#> 
#> $percent
#> [1] 0
#> 
#> $sin
#> [1] 0
#> 
#> $sout
#> [1] 0
#> 
```
