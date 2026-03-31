# Query the type of the OS

Query the type of the OS

## Usage

``` r
ps_os_type()

ps_is_supported()
```

## Value

`ps_os_type` returns a named logical vector. The rest of the functions
return a logical scalar.

`ps_is_supported()` returns `TRUE` if ps supports the current platform.

## Examples

``` r
ps_os_type()
#>   POSIX WINDOWS   LINUX   MACOS 
#>    TRUE   FALSE    TRUE   FALSE 
ps_is_supported()
#> [1] TRUE
```
