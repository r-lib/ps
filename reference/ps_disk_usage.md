# Disk usage statistics, per partition

The output is similar to the Unix `df` command.

## Usage

``` r
ps_disk_usage(paths = ps_disk_partitions()$mountpoint)
```

## Arguments

- paths:

  The mounted file systems to list. By default all file systems returned
  by
  [`ps_disk_partitions()`](https://ps.r-lib.org/reference/ps_disk_partitions.md)
  is listed.

## Value

A data frame with columns `mountpoint`, `total`, `used`, `available` and
`capacity`.

## Details

Note that on Unix a small percentage of the disk space (5% typically) is
reserved for the superuser. `ps_disk_usage()` returns the space
available to the calling user.

## See also

Other disk functions:
[`ps_disk_io_counters()`](https://ps.r-lib.org/reference/ps_disk_io_counters.md),
[`ps_disk_partitions()`](https://ps.r-lib.org/reference/ps_disk_partitions.md)

## Examples

``` r
ps_disk_usage()
#> # A data frame: 3 × 5
#>   mountpoint        total        used   available capacity
#>   <chr>             <dbl>       <dbl>       <dbl>    <dbl>
#> 1 /          154894188544 60563976192 94346989568   0.391 
#> 2 /boot         923156480   131149824   856649728   0.133 
#> 3 /boot/efi     109395456     6399488   102995968   0.0585
```
