# List all mounted partitions

The output is similar the Unix `mount` and `df` commands.

## Usage

``` r
ps_disk_partitions(all = FALSE)
```

## Arguments

- all:

  Whether to list virtual devices as well. If `FALSE`, on Linux it will
  still list `overlay` and `grpcfuse` file systems, to provide some
  useful information in Docker containers.

## Value

A data frame with columns `device`, `mountpoint`, `fstype` and
`options`.

## See also

Other disk functions:
[`ps_disk_io_counters()`](https://ps.r-lib.org/reference/ps_disk_io_counters.md),
[`ps_disk_usage()`](https://ps.r-lib.org/reference/ps_disk_usage.md)

## Examples

``` r
ps_disk_partitions(all = TRUE)
#> # A data frame: 24 × 4
#>    device     mountpoint           fstype     options                  
#>    <chr>      <chr>                <chr>      <chr>                    
#>  1 /dev/root  /                    ext4       rw,relatime,discard,erro…
#>  2 devtmpfs   /dev                 devtmpfs   rw,nosuid,noexec,relatim…
#>  3 proc       /proc                proc       rw,nosuid,nodev,noexec,r…
#>  4 sysfs      /sys                 sysfs      rw,nosuid,nodev,noexec,r…
#>  5 securityfs /sys/kernel/security securityfs rw,nosuid,nodev,noexec,r…
#>  6 tmpfs      /dev/shm             tmpfs      rw,nosuid,nodev,inode64  
#>  7 devpts     /dev/pts             devpts     rw,nosuid,noexec,relatim…
#>  8 tmpfs      /run                 tmpfs      rw,nosuid,nodev,size=327…
#>  9 tmpfs      /run/lock            tmpfs      rw,nosuid,nodev,noexec,r…
#> 10 cgroup2    /sys/fs/cgroup       cgroup2    rw,nosuid,nodev,noexec,r…
#> # ℹ 14 more rows
ps_disk_partitions()
#> # A data frame: 3 × 4
#>   device     mountpoint fstype options                                 
#> * <chr>      <chr>      <chr>  <chr>                                   
#> 1 /dev/root  /          ext4   rw,relatime,discard,errors=remount-ro,c…
#> 2 /dev/sda16 /boot      ext4   rw,relatime,discard                     
#> 3 /dev/sda15 /boot/efi  vfat   rw,relatime,fmask=0077,dmask=0077,codep…
```
