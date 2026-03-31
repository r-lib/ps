# Query the size of the current terminal

If the standard output of the current R process is not a terminal, e.g.
because it is redirected to a file, or the R process is running in a
GUI, then it will throw an error. You need to handle this error if you
want to use this function in a package.

## Usage

``` r
ps_tty_size()
```

## Details

If an error happens, the error message is different depending on what
type of device the standard output is. Some common error messages are:

- "Inappropriate ioctl for device."

- "Operation not supported on socket."

- "Operation not supported by device."

Whatever the error message, `ps_tty_size` always fails with an error of
class `ps_unknown_tty_size`, which you can catch.

## Examples

``` r
# An example that falls back to the 'width' option
tryCatch(
  ps_tty_size(),
  ps_unknown_tty_size = function(err) {
    c(width = getOption("width"), height = NA_integer_)
  }
)
#>  width height 
#>     71     NA 
```
