# Find the mount point of a file or directory

Find the mount point of a file or directory

## Usage

``` r
ps_fs_mount_point(paths)
```

## Arguments

- paths:

  Paths to files, directories, devices, etc. They must exist. They are
  normalized using
  [`base::normalizePath()`](https://rdrr.io/r/base/normalizePath.html).

## Value

Character vector, paths to the mount points of the input `paths`.

## Examples

``` r
ps_fs_mount_point(".")
#> [1] "/"
```
