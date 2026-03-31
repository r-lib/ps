# Number of logical or physical CPUs

If cannot be determined, it returns `NA`. It also returns `NA` on older
Windows systems, e.g. Vista or older and Windows Server 2008 or older.

## Usage

``` r
ps_cpu_count(logical = TRUE)
```

## Arguments

- logical:

  Whether to count logical CPUs.

## Value

Integer scalar.

## Examples

``` r
ps_cpu_count(logical = TRUE)
#> [1] 4
ps_cpu_count(logical = FALSE)
#> [1] 2
```
