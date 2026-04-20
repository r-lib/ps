# File status

This function is currently not implemented on Windows.

## Usage

``` r
ps_fs_stat(paths, follow = TRUE)
```

## Arguments

- paths:

  Paths to files, directories, devices, etc. They must exist. They are
  expanded using
  [`base::path.expand()`](https://rdrr.io/r/base/path.expand.html).

- follow:

  Whether to follow symbolic links. If `FALSE` it returns information on
  the links themselves.

## Value

Data frame with one row for each path in `paths`. Columns:

- `path`: Expanded `paths`.

- `dev_major`: Major device ID of the device the path resides on.

- `dev_minor`: Minor device ID of the device the path resodes on.

- `inode`: Inode number.

- `mode`: File type and mode (permissions). It is easier to use the
  `type` and `permissions` columns.

- `type`: File type, character. One of regular file, directory,
  character device, block device, FIFO, symbolic link, socket.

- `permissions`: Permissions, numeric code in an integer column.

- `nlink`: Number of hard links.

- `uid`: User id of owner.

- `gid`: Group id of owner.

- `rdev_major`: If the path is a device, its major device id, otherwise
  `NA_integer_`.

- `rdev_minor`: IF the path is a device, its minor device id, otherwise
  `NA_integer_`.

- `size`: File size in bytes.

- `block_size`: Block size for filesystem I/O.

- `blocks`: Number of 512B blocks allocated.

- `access_time`: Time of last access.

- `modification_time`: Time of last modification.

- `change_time`: Time of last status change.

## Examples

``` r
ps_fs_stat(c(".", tempdir()))
#> # A data frame: 2 × 18
#>   path   dev_major dev_minor  inode  mode type  permissions nlink   uid
#>   <chr>      <int>     <int>  <dbl> <dbl> <chr>       <int> <dbl> <dbl>
#> 1 .              8         1 8.67e6 16877 dire…         493     2  1001
#> 2 /tmp/…         8         1 8.66e6 16832 dire…         448     8  1001
#> # ℹ 9 more variables: gid <dbl>, rdev_major <int>, rdev_minor <int>,
#> #   size <dbl>, block_size <dbl>, blocks <dbl>, access_time <dttm>,
#> #   modification_time <dttm>, change_time <dttm>
```
