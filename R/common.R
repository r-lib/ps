
not_implemented_function <- function(...) {
  stop(ps__not_implemented())
}

assert_pid_not_reused <- function(fun) {
  fun
  function(...) {
    if (!self$is_running()) {
      stop(ps__no_such_process(self$.pid, self$.name))
    }
    fun(...)
  }
}

#' @importFrom R6 R6Class
#' @importFrom utils head tail

process_common <- function() {
  if (is.null(ps_env$process_common)) {
    ps_env$process_common <- R6Class(
      "process_common",
      cloneable = FALSE,
      public = list(

        initialize = function(pid = NULL) self$.init(pid),

        pid = function() self$.pid,

        format = function(...) {
          info <- list()
          info$pid <- self$.pid
          tryCatch({
            info$name <- self$name()
            if (!is.null(self$.create_time))
              info$create_time <- format(self$.create_time) },
            zombie_process = function(e) info$status <<- "zombie",
            no_such_process = function(e) info$status <<- "terminated",
            access_denied = function(e) e
          )

          paste0(
            "ps::process, ",
            paste(names(info), unlist(info), sep = "=", collapse = ", "))
        },

        print = function(...) {
          cat(self$format(...), sep =  "", "\n")
          invisible(self)
        },

        parent = function() {
          ppid <- self$ppid()
          if (!is.null(ppid)) {
            ctime <- self$create_time()
            tryCatch({
              parent <- process(ppid)
              if (parent$create_time() <= ctime) return(parent) },
              no_such_process = function(e) e)
          }
        },

        is_running = function() {
          if (self$.gone) return(FALSE)
          tryCatch(
            process(self$.pid)$create_time() == self$create_time(),
            zombie_process = function(e) TRUE,
            no_such_processs = function(e) {
              self$.gone <- TRUE
              FALSE
            }
          )
        },

        ppid = not_implemented_function,

        name = not_implemented_function,

        exe = function() {
          cmdline <- self$cmdline()
          exe <-  cmdline[[1]]
          if (path_is_absolute(exe) && file.exists(exe)) {
            ## TODO: check if executable
            return(exe)
          }
        },

        cmdline = not_implemented_function,

        status = not_implemented_function,

        username = not_implemented_function,

        create_time = not_implemented_function,

        cwd = not_implemented_function,

        ## nice = not_implemented_function,

        uids = not_implemented_function,

        gids = not_implemented_function,

        terminal = not_implemented_function,

        ## num_fds = not_implemented_function,

        ## io_counters = not_implemented_function,

        ## ionice_get = not_implemented_function,

        ## rlimit = not_implemented_function,

        ## cpu_affinity_get = not_implemented_function,

        ## cpu_num = not_implemented_function,

        environ = not_implemented_function,

        ## num_handles = not_implemented_function,

        ## num_ctx_switches = not_implemented_function,

        num_threads = not_implemented_function,

        ## threads = not_implemented_function,

        children = decorator(assert_pid_not_reused, function(recursive = FALSE) {
          assert_that(is_flag(recursive))
          map <- ps_ppid_map()
          ret <- list()
          if (!recursive) {
            for (i in seq_len(nrow(map))) {
              if (map$ppid[i] == self$.pid) {
                tryCatch({
                  child  <- process(map$pid[i])
                  if (self$create_time() <= child$create_time()) {
                    ret <- c(ret, child)
                  } },
                  no_such_process = function(e) NULL,
                  zombie_process = function(e) NULL)
              }
            }

          } else {
            seen <- integer()
            stack <- self$.pid
            while (length(stack)) {
              pid <- tail(stack, 1)
              stack <- head(stack, -1)
              if (pid %in% seen) next
              seen <- c(seen, pid)
              child_pids <- map[ map[,2] ==  pid, 1]
              for (child_pid in child_pids) {
                tryCatch({
                  child = process(child_pid)
                  if (self$create_time() <= child$create_time()) {
                    ret <- c(ret, child)
                    stack <- c(stack, child_pid)
                  } },
                  no_such_process = function(e) NULL,
                  zombie_process = function(e) NULL
                )
              }
            }
          }

          ret
        }),

        ## cpu_percent = not_implemented_function,

        cpu_times = not_implemented_function,

        memory_info = not_implemented_function,

        ## memory_full_info = not_implemented_function,

        ## memory_percent = not_implemented_function,

        ## memory_maps = not_implemented_function,

        ## open_files = not_implemented_function,

        ## connections = not_implemented_function,

        send_signal = not_implemented_function,

        suspend = not_implemented_function,

        resume = not_implemented_function,

        terminate = not_implemented_function,

        kill = not_implemented_function,

        ## wait = not_implemented_function,

        ## Internal methods

        .init = function(pid, ignore_nsp = FALSE) {
          decorate(self)
          pid <- pid %||% Sys.getpid()
          assert_that(is_pid(pid))
          self$.pid <- as.integer(pid)
          tryCatch(
            self$.create_time <- self$create_time(),
            ## We should never get here as AFAIK we're able to get
            ## process creation time on all platforms even as a
            ## limited user.
            access_denied = function(e) e,
            ## Zombies can still be queried by this class (although
            ## not always) and pids() return them so just go on.
            zombie_process = function(e) e,
            no_such_process = function(e) {
              if (!ignore_nsp) {
                e$message <- paste("no process found with pid", pid)
                stop(e)
              } else {
                self$.gone <- TRUE
              }
            }
          )
        },

        ## Internal methods
        .common_pcputimes = function(values) {
          values <- as.numeric(values)
          names(values) <-
            c("user", "system", "children_user", "children_system")
          values
        },

        .oneshot_enter = function() NULL,

        .oneshot_exit = function() NULL,

        ## Internal data
        .pid = NULL,
        .name = NULL,
        .exe = NULL,
        .create_time = NULL,
        .gone = FALSE,
        .last_sys_cpu_times = NULL,
        .last_proc_cpu_times = NULL
      )
    )
  }

  ps_env$process_common
}

#' Query process properties with caching
#'
#' Avoid quering process information multiple time, as much as possible.
#'
#' @param process Process object to cache information for.
#' @param expr Expression to evaluate in the context of the cache.
#'   Typically it contains `process` queries.
#' @return Value of `expr`.
#'
#' @export

with_process <- function(process, expr) {
  process$.oneshot_enter()
  on.exit(process$.oneshot_exit(), add = TRUE)
  expr
}
