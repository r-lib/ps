# Wait for one or more processes to terminate, with a timeout

This function supports interruption with SIGINT on Unix, or CTRL+C or
CTRL+BREAK on Windows.

## Usage

``` r
ps_wait(p, timeout = -1)
```

## Arguments

- p:

  A process handle, or a list of process handles. The process(es) to
  wait for.

- timeout:

  Timeout in milliseconds. If -1, `ps_wait()` will wait indefinitely (or
  until it is interrupted). If 0, then it checks which processes have
  already terminated, and returns immediately.

## Value

Logical vector, with one value of each process in `p`. For processes
that terminated it contains a `TRUE` value. For processes that are still
running it contains a `FALSE` value.

## Examples

``` r
# this example calls `sleep`, so it only works on Unix
p1 <- processx::process$new("sleep", "100")
p2 <- processx::process$new("sleep", "100")

# returns c(FALSE, FALSE) immediately if p1 and p2 are running
ps_wait(list(p1$as_ps_handle(), p2$as_ps_handle()), 0)
#> [1] FALSE FALSE

# timeouts at one second
ps_wait(list(p1$as_ps_handle(), p2$as_ps_handle()), 1000)
#> [1] FALSE FALSE

p1$kill()
#> [1] TRUE
p2$kill()
#> [1] TRUE
# returns c(TRUE, TRUE) immediately
ps_wait(list(p1$as_ps_handle(), p2$as_ps_handle()), 1000)
#> [1] TRUE TRUE
```
