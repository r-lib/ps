
# ps

> List system
processes

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/r-lib/ps.svg?branch=master)](https://travis-ci.org/r-lib/ps)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/r-lib/ps?branch=master&svg=true)](https://ci.appveyor.com/project/gaborcsardi/ps)
[![CRAN
status](https://www.r-pkg.org/badges/version/ps)](https://cran.r-project.org/package=ps)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/ps)](https://www.r-pkg.org/pkg/ps)
[![Coverage
status](https://codecov.io/gh/r-lib/ps/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/ps?branch=master)

## Installation

Once released, you can install the released version of ps from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ps")
```

## Listing all processes

`ps_pids()` returns all process ids on the system. This can be useful to
iterate over all processes.

``` r
library(ps)
ps_pids()[1:20]
```

    ##  [1]  0  1 48 49 51 52 53 55 58 59 60 61 66 70 72 78 79 81 83 84

`ps()` is similar, but it returns a data frame (tibble if you have the
tibble package available), with data about each process:

``` r
ps()
```

    ## # A tibble: 353 x 10
    ##      pid  ppid name  username status   user system    rss    vms
    ##  * <int> <int> <chr> <chr>    <chr>   <dbl>  <dbl>  <dbl>  <dbl>
    ##  1 15310     1 mdwo… gaborcs… runni… 0.0321 0.0144 1.94e7 2.54e9
    ##  2 15309     1 mdwo… gaborcs… runni… 0.0357 0.0176 1.95e7 2.54e9
    ##  3 15308     1 mdwo… gaborcs… runni… 0.0320 0.0142 1.85e7 2.54e9
    ##  4 15307     1 mdwo… gaborcs… runni… 0.0318 0.0138 1.85e7 2.54e9
    ##  5 15306     1 mdwo… gaborcs… runni… 0.0341 0.0173 1.95e7 2.54e9
    ##  6 15305     1 mdwo… gaborcs… runni… 0.0393 0.0209 1.98e7 2.54e9
    ##  7 15304     1 mdwo… gaborcs… runni… 0.0345 0.0183 1.97e7 2.54e9
    ##  8 15303     1 mdwo… gaborcs… runni… 0.0370 0.0203 1.99e7 2.54e9
    ##  9 15302     1 mdwo… gaborcs… runni… 0.0383 0.0215 1.99e7 2.54e9
    ## 10 15301     1 mdwo… gaborcs… runni… 0.0371 0.0206 1.98e7 2.54e9
    ## # ... with 343 more rows, and 1 more variable: created <dttm>

## Process API

TODO

## Low level process API

TODO

## Process cleanup

TODO

## processx and ps

TODO

## Contributions

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.

## License

BSD © RStudio
