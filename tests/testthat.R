library(testthat)
library(ps)

if (ps::ps_is_supported() && Sys.getenv("R_COVR", "") != "true") {
  reporter <- ps::CleanupReporter(testthat::SummaryReporter)$new()
} else {
  ## ps does not support this platform
  reporter <- "progress"
}

if (ps_is_supported()) test_check("ps", reporter = reporter)
