
globalVariables("private")

#' testthat reporter that checks if child proceses are cleaned up in tests
#'
#' `CleanupReporter` takes an existing testthat `Reporter` object, and
#' wraps it, so it checks for leftover child processes, at the specified
#' place, see the `unit` argument below.
#'
#' Child processes can be reported via a failed expectation, cleaned up
#' silently, or cleaned up and reptorted (the default).
#'
#' The constructor of the `CleanupReporter` class has options:
#' * `file`: the output file, if any, this is passed to `reporter`.
#' * `unit`: when to perform the child process check and cleanup. Possible
#'   values:
#'     * `"test"`: at the end of each [testthat::test_that()] block
#'       (the default),
#'     * `"testsuite"`: at the end of the test suite.
#' * `process_cleanup`: Logical scalar, whether to kill the leftover
#'   processes.
#' * `process_fail`: Whether to create an expectation, that fails if there
#'   are any processes alive.
#'
#' @note Some IDEs, like RStudio, start child processes frequently, and
#' sometimes crash when these are killed, only use this reporter in a
#' terminal session. In particular, you can always use it in the
#' idiomatic `testthat.R` file, that calls `test_check()` during
#' `R CMD check`.
#'
#' @param reporter A testthat reporter to wrap into a new `CleanupReporter`
#'   class.
#' @return New reporter class  that behaves exactly like `reporter`,
#'   but it checks for, and optionally cleans up child processes, at the
#'   specified granularity.
#'
#' @section Examples:
#' This is how to use this reporter in `testthat.R`:
#' ```
#' library(testthat)
#' library(mypackage)
#'
#' if  (ps::ps_is_supported()) {
#'   reporter <- ps::CleanupReporter(testthat::ProgressReporter)$new(
#'     unit = "test", process_cleanup = TRUE)
#' } else {
#'   ## ps does not support this platform
#'   reporter <- "progress"
#' }
#'
#' test_check("mypackage", reporter = reporter)
#' ```
#'
#' @export

CleanupReporter <- function(reporter = testthat::ProgressReporter) {

  R6::R6Class("CleanupReporter",
    inherit = reporter,
    public = list(

      initialize = function(
        file = getOption("testthat.output_file", stdout()),
        unit = c("test", "testsuite"),
        process_cleanup = TRUE, process_fail = TRUE) {

        if (!ps::ps_is_supported()) {
          stop("CleanupReporter is not supported on this platform")
        }

        super$initialize(file = file)
        private$unit <- match.arg(unit)
        private$process_cleanup <- process_cleanup
        private$process_fail <- process_fail

        invisible(self)
      },

      start_test = function(context, test) {
        super$start_test(context, test)
        if (private$unit == "test") private$tree_id <- ps::ps_mark_tree()
      },

      end_test = function(context, test) {
        if (private$unit == "test" && !is.null(private$tree_id)) {
          self$cleanup(test)
        }
        super$end_test(context, test)
      },

      start_reporter = function() {
        super$start_reporter()
        if (private$unit == "testsuite") private$tree_id <- ps::ps_mark_tree()
      },

      end_reporter = function() {
        super$end_reporter()
        if (private$unit  == "testsuite" && !is.null(private$tree_id)) {
          self$cleanup("testsuite", quote = "")
        }
      },

      cleanup = function(test, quote = "'") {
        Sys.unsetenv(private$tree_id)
        if (private$process_cleanup) {
          ret <- ps::ps_kill_tree(private$tree_id)
        } else {
          ret <- ps::ps_find_tree(private$tree_id)
        }
        if (private$process_fail)  {
          testthat::with_reporter(self, start_end_reporter = FALSE, {
            self$expect_cleanup(test, ret, quote)
          })
        }
      },

      expect_cleanup = function(test, pids, quote) {
        act <- testthat::quasi_label(rlang::enquo(test), test)
        act$pids <- length(pids)
        testthat::expect(
          length(pids) == 0,
          sprintf("%s did not clean up processes: %s", act$lab,
                  paste0(encodeString(names(pids), quote = quote),
                         " (", pids, ")", collapse = ", ")))

        invisible(act$val)
      }
    ),

    private = list(
      unit = NULL,
      process_cleanup = NULL,
      process_fail = NULL,
      tree_id = NULL
    )
  )
}
