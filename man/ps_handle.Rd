% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/low-level.R
\name{ps_handle}
\alias{ps_handle}
\alias{as.character.ps_handle}
\alias{format.ps_handle}
\alias{print.ps_handle}
\title{Create a process handle}
\usage{
ps_handle(pid = NULL, time = NULL)

\method{as.character}{ps_handle}(x, ...)

\method{format}{ps_handle}(x, ...)

\method{print}{ps_handle}(x, ...)
}
\arguments{
\item{pid}{A process id (integer scalar) or process string (from
\code{ps_string()}). \code{NULL} means the current R process.}

\item{time}{Start time of the process. Usually \code{NULL} and ps will query
the start time.}

\item{x}{Process handle.}

\item{...}{Not used currently.}
}
\value{
\code{ps_handle()} returns a process handle (class \code{ps_handle}).
}
\description{
Create a process handle
}
\examples{
\dontshow{if (ps::ps_is_supported() && ! ps:::is_cran_check()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
p <- ps_handle()
p
\dontshow{\}) # examplesIf}
}
\seealso{
Other process handle functions: 
\code{\link{ps_children}()},
\code{\link{ps_cmdline}()},
\code{\link{ps_connections}()},
\code{\link{ps_cpu_times}()},
\code{\link{ps_create_time}()},
\code{\link{ps_cwd}()},
\code{\link{ps_descent}()},
\code{\link{ps_environ}()},
\code{\link{ps_exe}()},
\code{\link{ps_interrupt}()},
\code{\link{ps_is_running}()},
\code{\link{ps_kill}()},
\code{\link{ps_memory_info}()},
\code{\link{ps_name}()},
\code{\link{ps_num_fds}()},
\code{\link{ps_num_threads}()},
\code{\link{ps_open_files}()},
\code{\link{ps_pid}()},
\code{\link{ps_ppid}()},
\code{\link{ps_resume}()},
\code{\link{ps_send_signal}()},
\code{\link{ps_shared_libs}()},
\code{\link{ps_status}()},
\code{\link{ps_suspend}()},
\code{\link{ps_terminal}()},
\code{\link{ps_terminate}()},
\code{\link{ps_uids}()},
\code{\link{ps_username}()}
}
\concept{process handle functions}
