library(testthat)
library(ps)

if  (any(ps_os_type()[c("WINDOWS", "MACOS", "LINUX")]))  {
  test_check("ps", reporter = "summary")
}
