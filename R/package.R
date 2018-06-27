
ps_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  ps_env$constants <- new.env(parent  = emptyenv())
  .Call(ps__init, asNamespace("ps"), ps_env$constants)
}

utils::globalVariables(unique(c(
  "self",

  ## POSIX
  "ps__pid_exists",
  "ps__get_pw_uid",
  "ps__kill",
  "ps__stat_st_rdev",

  ## OSX
  "ps__pids",
  "ps__proc_exe",
  "ps__proc_cmdline",
  "ps__proc_environ",
  "ps__proc_cwd",
  "ps__proc_kinfo_oneshot",
  "ps__proc_pidtaskinfo_oneshot",

  ## LINUX
  "ps__readlink",
  "ps__linux_clk_tck",
  "ps__linux_pagesize",

  ## WINDOWS
  "ps__pids",
  "ps__ppid_map",
  "ps__pid_exists",
  "ps__boot_time",
  "ps__proc_name",
  "ps__proc_exe",
  "ps__proc_cmdline",
  "ps__proc_environ",
  "ps__proc_cwd",
  "ps__proc_username",
  "ps__proc_info",
  "ps__proc_memory_info",
  "ps__proc_cpu_times",
  "ps__proc_create_time",
  "ps__proc_is_suspended",
  "ps__win32_QueryDosDevice"
)))
