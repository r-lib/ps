% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system.R
\name{ps_cpu_count}
\alias{ps_cpu_count}
\title{Number of logical or physical CPUs}
\usage{
ps_cpu_count(logical = TRUE)
}
\arguments{
\item{logical}{Whether to count logical CPUs.}
}
\value{
Integer scalar.
}
\description{
If cannot be determined, it returns \code{NA}. It also returns \code{NA} on older
Windows systems, e.g. Vista or older and Windows Server 2008 or older.
}
\examples{
\dontshow{if (ps::ps_is_supported() && ! ps:::is_cran_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
ps_cpu_count(logical = TRUE)
ps_cpu_count(logical = FALSE)
\dontshow{\}) # examplesIf}
}
