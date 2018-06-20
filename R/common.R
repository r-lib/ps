
get_procfs_path <- function() {
  os <- ps_os_type()
  if (os[["LINUX"]] || os[["SUNOS"]] || os[["AIX"]]) {
      "/proc"
  }
}

common_puids <- function(values) {
  values <- as.integer(values)
  names(values) <- c("real", "effective", "saved")
  values
}

common_pcputimes <- function(values) {
  values <- as.numeric(values)
  names(values) <-
    c("user", "system", "children_user", "children_system")
  values
}
