# System CPU times.

Every attribute represents the seconds the CPU has spent in the given
mode. The attributes availability varies depending on the platform:

- `user`: time spent by normal processes executing in user mode; on
  Linux this also includes guest time.

- `system`: time spent by processes executing in kernel mode.

- `idle`: time spent doing nothing.

## Usage

``` r
ps_system_cpu_times()
```

## Value

Named list

## Details

Platform-specific fields:

- `nice` (UNIX): time spent by niced (prioritized) processes executing
  in user mode; on Linux this also includes guest_nice time.

- `iowait` (Linux): time spent waiting for I/O to complete. This is not
  accounted in idle time counter.

- `irq` (Linux): time spent for servicing hardware interrupts.

- `softirq` (Linux): time spent for servicing software interrupts.

- `steal` (Linux 2.6.11+): time spent by other operating systems running
  in a virtualized environment.

- `guest` (Linux 2.6.24+): time spent running a virtual CPU for guest
  operating systems under the control of the Linux kernel.

- `guest_nice` (Linux 3.2.0+): time spent running a niced guest (virtual
  CPU for guest operating systems under the control of the Linux
  kernel).

## Examples

``` r
ps_system_cpu_times()
#>       user       nice     system       idle     iowait        irq 
#>      94.07       5.16      42.69    5312.25      12.37       0.00 
#>    softirq      steal      guest guest_nice 
#>       0.97       0.00       0.00       0.00 
```
