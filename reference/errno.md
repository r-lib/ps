# List of 'errno' error codes

For the errors that are not used on the current platform, `value` is
`NA_integer_`.

## Usage

``` r
errno()
```

## Details

A data frame with columns: `name`, `value`, `description`.

## Examples

``` r
errno()
#> # A data frame: 156 × 3
#>    name          value description                                     
#>    <chr>         <int> <chr>                                           
#>  1 E2BIG             7 Arg list too long.                              
#>  2 EACCES           13 Permission denied.                              
#>  3 EADDRINUSE       98 Address already in use.                         
#>  4 EADDRNOTAVAIL    99 Cannot assign requested address.                
#>  5 EADV             68 Advertise error.                                
#>  6 EAFNOSUPPORT     97 Address family not supported by protocol family.
#>  7 EAGAIN           11 Resource temporarily unavailable.               
#>  8 EALREADY        114 Operation already in progress.                  
#>  9 EAUTH            NA Authentication error.                           
#> 10 EBACKGROUND      NA Caller not in the foreground process group      
#> # ℹ 146 more rows
```
