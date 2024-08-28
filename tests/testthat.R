library(testthat)
library(ps)

if (ps::ps_is_supported() && Sys.getenv("R_COVR", "") != "true" &&
    Sys.getenv("NOT_CRAN") != "" ) {
  reporter <- ps::CleanupReporter(testthat::SummaryReporter)$new()
} else {
  reporter <- "summary"
}

if (ps_is_supported()) test_check("ps", reporter = reporter)
