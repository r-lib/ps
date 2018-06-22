
ps_pids_windows <- function() {
  sort(.Call(ps__pids))
}

ps_pid_exists_windows <- function(pid) {
  .Call(ps__pid_exists, as.integer(pid))
}

ps_boot_time_windows <- function() {
  if (is.null(ps_env$boot_time)) {
    bt <- .Call(ps__boot_time)
    ps_env$boot_time <- as.POSIXct(bt, origin = "1970-01-01", tz = "GMT")
  }
  ps_env$boot_time
}

#' @importFrom R6 R6Class

process_windows <- R6Class(
  "process_windows",
  cloneable = FALSE,
  public = list(
    initialize = function(pid)
      p_windows_init(self, private, pid),
    get_name = function()
      p_windows_get_name(self, private),
    get_exe = function()
      p_windows_get_exe(self, private),
    get_cmdline = function()
      p_windows_get_cmdline(self, private),
    get_environ = function(cached = TRUE)
      p_windows_get_environ(self, private, cached),
    get_ppid = function()
      p_windows_get_ppid(self, private),
    get_cwd = function()
      p_windows_get_cwd(self, private),
    get_username = function()
      p_windows_get_username(self, private),
    get_memory_info = function()
      p_windows_get_memory_info(self, private),
    get_cpu_times = function()
      p_windows_get_cpu_times(self, private),
    get_create_time = function()
      p_windows_get_create_time(self, private),
    get_num_threads = function()
      p_windows_get_num_threads (self, private)
  ),

  private = list(
    pid = NULL,
    ppid = NULL,
    name = NULL,
    start = NULL,
    environ = NULL,
    oneshot_data = NULL,

    oneshot_info = function(cached = TRUE)
      p_windows__oneshot_info(self, private, cached),
    get_raw_meminfo = function()
      p_windows__get_raw_meminfo(self, private)
  )
)

p_windows_init <- function(self, private, pid) {
  assert_that(is_pid(pid))
  private$pid <- as.integer(pid)
  invisible(self)
}

p_windows_get_name <- function(self, private) {
  ## This is how PIDs 0 and 4 are always represented in taskmgr
  ## and process-hacker.
  if (private$pid == 0L) {
    "System Idle Process"
  } else if (private$pid == 4L) {
    "System"
  }  else {
    tryCatch(
      ## Note: this will fail with AD for most PIDs owned
      ## by another user but it's faster.
      basename(self$get_exe()),
      error = function(e) {
        .Call(ps__proc_name, as.integer(private$pid))
      }
    )
  }
}

p_windows_get_exe <- function(self, private) {
  ## Note: os.path.exists(path) may return False even if the file
  ## is there, see:
  ## http://stackoverflow.com/questions/3112546/os-path-exists-lies
  
  ## see https://github.com/giampaolo/psutil/issues/414
  ## see https://github.com/giampaolo/psutil/issues/528
  if (private$pid == 0L || private$pid == 4L) stop("Access denied")
  convert_dos_path(.Call(ps__proc_exe, private$pid))
}

p_windows_get_cmdline <- function(self, private) {
  .Call(ps__proc_cmdline, as.integer(private$pid))
}

p_windows_get_environ <- function(self, private, cached) {
  if (is.null(private$environ) || !cached) {
    private$environ <- parse_envs(.Call(ps__proc_environ, private$pid))
  }
  private$environ
}

p_windows_get_ppid <- function(self, private) {
  map <- windows_ppid_map()
  idx <- match(private$pid, map[,1])
  if (is.na(idx)) stop("Process does not exist")
  map[idx, 2]
}

p_windows_get_cwd <- function(self, private) {
  if (private$pid == 0L || private$pid == 4L) {
    stop("Access denied")
  }
  sub("\\\\$", "", .Call(ps__proc_cwd, private$pid))
}

p_windows_get_username <- function(self, private) {
  if (private$pid == 0L || private$pid == 4L) {
    "NT AUTHORITY\\SYSTEM"
  } else {
    domain_user <- .Call(ps__proc_username, private$pid)
    paste0(domain_user[[1]], "\\", domain_user[[2]])
  }
}

p_windows_get_memory_info <- function(self, private) {
  t <- private$get_raw_meminfo()
  c(list(rss = t[["wset"]], vms = t[["pagefile"]]), t)
}

p_windows_get_cpu_times <- function(self, private) {
  tryCatch(
    .Call(ps__proc_cpu_times, private$pid),
    error = function(e) {
      info <- private$oneshot_info(cached = FALSE)
      common_pcputimes(c(info[["user_time"]], info[["kernel_time"]], 0, 0))
    })
}

p_windows_get_create_time <- function(self, private) {
  if (private$pid == 0L || private$pid == 4L) {
    ps_boot_time()
  } else {
    tryCatch(
      .Call(ps__proc_create_time, private$pid),
      error = function(e) {
        ct <- private$oneshot_info()[["create_time"]]
        as.POSIXct(ct, origin = "1970-01-01", tz = "GMT")
      })
  }
}

p_windows_get_num_threads <- function(self, private) {
  private$oneshot_info(cached = FALSE)[["num_threads"]]
}

p_windows__oneshot_info <- function(self, private, cached) {
  if (!cached || is.null(private$oneshot_data)) {
    private$oneshot_data <- .Call(ps__proc_info, private$pid)
  }
  private$oneshot_data
}

p_windows__get_raw_meminfo <- function(self, private) {
  tryCatch(
    .Call(ps__proc_memory_info, private$pid),
    error = function(e) {
      info <- private$oneshot_info()
      info[c("num_page_faults", "peak_wset", "wset", "peak_paged_pool",
             "paged_pool", "peak_non_paged_pool", "non_paged_pool",
             "pagefile", "peak_pagefile", "mem_private")]
    })
}

#' @importFrom utils head

convert_dos_path <- function(path) {
  pcs <- strsplit(path, "\\", fixed =  TRUE)[[1]]
  rawdrive <- paste(head(pcs, 3), collapse = "\\")
  driveletter <- .Call(ps__win32_QueryDosDevice, rawdrive)
  paste0(driveletter, substr(path, nchar(rawdrive) + 1, nchar(path)))
}

windows_ppid_map <- function() {
  pids <- .Call(ps__ppid_map)
  pidx <- seq(2L, length(pids), by = 2L)
  data.frame(
    pid = pids[pidx],
    ppid = pids[pidx-1L]
  )
}
