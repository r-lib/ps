# System-wide disk I/O counters

Returns a data.frame of system-wide disk I/O counters.

## Usage

``` r
ps_disk_io_counters()
```

## Value

A data frame of one row per disk of I/O stats, with columns `name`,
`read_count` `read_merged_count` `read_bytes`, `read_time`,
`write_count`, `write_merged_count`, `write_bytes` `write_time`, and
`busy_time`.

## Details

Includes the following non-NA fields for all supported platforms:

- `read_count`: number of reads

- `write_count`: number of writes

- `read_bytes`: number of bytes read

- `write_bytes`: number of bytes written

And for only some platforms:

- `read_time`: time spent reading from disk (in milliseconds)

- `write_time`: time spent writing to disk (in milliseconds)

- `busy_time`: time spent doing actual I/Os (in milliseconds)

- `read_merged_count`: number of merged reads (see iostats doc)

- `write_merged_count`: number of merged writes (see iostats doc)

## See also

Other disk functions:
[`ps_disk_partitions()`](https://ps.r-lib.org/dev/reference/ps_disk_partitions.md),
[`ps_disk_usage()`](https://ps.r-lib.org/dev/reference/ps_disk_usage.md)

## Examples

``` r
ps_disk_io_counters()
#> # A data frame: 13 × 10
#>    name  read_count read_merged_count read_bytes read_time write_count
#>    <chr>      <dbl>             <dbl>      <dbl>     <dbl>       <dbl>
#>  1 loop0          0                28          0         0           0
#>  2 loop1          0                 0          0         0           0
#>  3 loop2          0                 0          0         0           0
#>  4 loop3          0                 0          0         0           0
#>  5 loop4          0                 0          0         0           0
#>  6 loop5          0                 0          0         0           0
#>  7 loop6          0                 0          0         0           0
#>  8 loop7          0                 0          0         0           0
#>  9 sda         6384           2111069   10567680     39056       91187
#> 10 sda1        6349           2074302   10430976     39027       91155
#> 11 sda14          0              1952      25600         0           0
#> 12 sda15          1             18848      35840         1           0
#> 13 sda16         34             11583      49664        23          24
#> # ℹ 4 more variables: write_merged_count <dbl>, write_bytes <dbl>,
#> #   write_time <dbl>, busy_time <dbl>
```
