
ps_pids_linux <- function() {
  sort(as.integer(dir(get_procfs_path(), pattern =  "^[0-9]+$")))
}

ps_pid_exists_linux <- function(pid) {
  if (! .Call(ps__pid_exists, as.integer(pid)))  {
    FALSE
  } else {
    ## Linux's apparently does not distinguish between PIDs and TIDs
    ## (thread IDs).
    ## listdir("/proc") won't show any TID (only PIDs) but
    ## os.stat("/proc/{tid}") will succeed if {tid} exists.
    ## os.kill() can also be passed a TID. This is quite confusing.
    ## In here we want to enforce this distinction and support PIDs
    ## only, see:
    ## https://github.com/giampaolo/psutil/issues/687
    tryCatch({
      ## Note: already checked that this is faster than using a
      ## regular expr. Also (a lot) faster than doing
      ## 'return pid in pids()'
      path <- sprintf("%s/%i/status", get_procfs_path(), pid)
      lines <- readLines(path)
      tgidline <- grep("^Tgid:", lines, value = TRUE)[1]
      tgid <- strsplit(tgidline, "\\s+")[[1]][2]
      identical(as.integer(tgid), as.integer(pid))
    },
    error = function() {
        pid %in% ps_pids()
    })
  }
}

ps_boot_time_linux <- function() {
  if (is.null(ps_env$boot_time)) {
    path <- sprintf("%s/stat", get_procfs_path())
    btime <- grep("btime", readLines(path), value = TRUE)
    bt <- as.numeric(strsplit(str_strip(btime), "\\s+")[[1]][[2]])
    ps_env$boot_time <- as.POSIXct(bt, origin = "1970-01-01", tz = "GMT")
  }
  ps_env$boot_time
}

#' @importFrom R6 R6Class

process_linux <- R6Class(
  "process_linux",
  cloneable = FALSE,
  public = list(
    initialize = function(pid)
      p_linux_init(self, private, pid),
    get_name = function()
      p_linux_get_name(self, private),
    get_exe = function()
      p_linux_get_exe(self, private),
    get_cmdline = function()
      p_linux_get_cmdline(self, private),
    get_environ = function(cached = TRUE)
      p_linux_get_environ(self, private, cached),
    get_ppid = function()
      p_linux_get_ppid(self, private),
    get_cwd = function()
      p_linux_get_cwd(self, private),
    get_uids = function()
      p_linux_get_uids(self, private),
    get_gids = function()
      p_linux_get_gids(self, private),
    get_memory_info = function()
      p_linux_get_memory_info(self, private),
    get_cpu_times = function()
      p_linux_get_cpu_times(self, private),
    get_create_time = function()
      p_linux_get_create_time(self, private),
    get_num_threads = function()
      p_linux_get_num_threads (self, private)

    ## TODO:
    ## - cpu_num (?)
    ## - terminal
    ## - wait
    ## - memory_full_info
    ## - num_ctx_switches
    ## - threads
    ## - nice_get
    ## - nice_set
    ## - cpu_affinity_get
    ## - cpu_affinity_set
    ## - status
    ## - open_files
    ## - connections
    ## - num_fds
    ## - memory_maps
    ## - ionice_get
    ## - ionice_set
    ## - rlimit
  ),

  private = list(
    pid = NULL,
    ppid = NULL,
    name = NULL,
    start = NULL,
    procfs_path = NULL,

    parse_stat_file = function()
      p_linux__parse_stat_file(self, private),
    read_status_file = function()
      p_linux__read_status_file(self, private)
  )
)

p_linux_init <- function(self, private, pid) {
  assert_that(is_pid(pid))
  private$pid <- as.integer(pid)
  private$procfs_path <- get_procfs_path()
  invisible(self)
}

p_linux_get_name <- function(self, private) {
  private$parse_stat_file()[[1]]
}

p_linux_get_exe <- function(self, private) {
  ## TODO: better errors, i.e. no such process or access denied
  readlink(sprintf("%s/%i/exe", private$procfs_path, private$pid))
}

p_linux_get_cmdline <- function(self, private) {
  path <- sprintf("%s/%i/cmdline", private$procfs_path, private$pid)
  data <- read_binary_file(path)

  ## May happen in case of a zombie process
  if (!length(data)) return(character())

  ## 'man proc' states that args are separated by null bytes '\0'
  ## and last char is supposed to be a null byte. Nevertheless
  ## some processes may change their cmdline after being started
  ## (via setproctitle() or similar), they are usually not
  ## compliant with this rule and use spaces instead. Google
  ## Chrome process is an example. See:
  ## https://github.com/giampaolo/psutil/issues/1179
  sep <-  if (data[length(data)] == 0x00) 0x00 else charToRaw(" ")
  map_chr(raw_split(data[-length(data)], sep), rawToChar)
}

p_linux_get_environ <- function(self, private, cached) {
  path <- sprintf("%s/%i/environ", private$procfs_path, private$pid)
  data <- read_binary_file(path)
  parse_envs(map_chr(raw_split(data[-length(data)], 0x00), rawToChar))
}

p_linux_get_ppid <- function(self, private) {
  as.integer(private$parse_stat_file()[[3]])
}

p_linux_get_cwd <- function(self, private) {
  ## TODO: better errors
  readlink(sprintf("%s/%i/cwd", private$procfs_path, private$pid))
}

p_linux_get_uids <- function(self, private) {
  status <- private$read_status_file()
  line <- grep("^Uid:", status, value = TRUE)[1]
  match <- re_match(line, "^Uid:\\t(\\d+)\\t(\\d+)\\t(\\d+)")
  common_puids(c(match[[1]], match[[2]], match[[3]]))
}

p_linux_get_gids <- function(self, private) {
  status <- private$read_status_file()
  line <- grep("^Gid:", status, value = TRUE)[1]
  match <- re_match(line, "^Gid:\\t(\\d+)\\t(\\d+)\\t(\\d+)")
  common_puids(c(match[[1]], match[[2]], match[[3]]))
}

p_linux_get_memory_info <- function(self, private) {
  path <- sprintf("%s/%i/statm", private$procfs_path, private$pid)
  mi <- scan(path, n = 7, quiet = TRUE)
  names(mi) <- c("rss", "vms", "shared", "text", "lib", "data", "dirty")
  mi
}

p_linux_get_cpu_times <- function(self, private) {
  stat <- private$parse_stat_file()
  common_pcputimes(as.numeric(stat[c(13:16)]) / linux_clock_ticks())
}

p_linux_get_create_time <- function(self, private) {
  stat <- private$parse_stat_file()
  bt <- ps_boot_time()
  bt + as.numeric(stat[[21]]) / linux_clock_ticks()
}

p_linux_get_num_threads <- function(self, private) {
  status <- private$read_status_file()
  line <- grep("^Threads:", status, value = TRUE)[1]
  match <- re_match(line, "^Threads:\\t(\\d+)")
  as.integer(match[[1]])
}

p_linux__parse_stat_file <- function(self, private) {
  path <- sprintf("%s/%i/stat", private$procfs_path, private$pid)
  stat <- paste(readLines(path), collapse = "\n")
  name <- sub("^.*[(](.*)[)].*$", "\\1", stat, perl = TRUE)
  fields <- strsplit(sub("^.*[)]\\s+", "", stat), "\\s+")[[1]]
  c(name, fields)
}

p_linux__read_status_file <- function(self, private) {
  path <-  sprintf("%s/%i/status", private$procfs_path, private$pid)
  readLines(path)
}

linux_clock_ticks <- function() {
  if (is.null(ps_env$clock_ticks)) {
    ps_env$clock_ticks <- .Call(ps__linux_clk_tck)
  }
  ps_env$clock_ticks
}

linux_pagesize <- function() {
  if (is.null(ps_env$pagesize)) {
    ps_env$pagesie  <- .Call(ps__linux_pagesize)
  }
  ps_env$pagesize
}
