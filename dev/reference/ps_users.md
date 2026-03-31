# List users connected to the system

List users connected to the system

## Usage

``` r
ps_users()
```

## Value

A data frame with columns `username`, `tty`, `hostname`, `start_time`,
`pid`. `tty` and `pid` are `NA` on Windows. `pid` is the process id of
the login process. For local users the `hostname` column is the empty
string.
