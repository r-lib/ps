
ps_cpu_count_physical_linux <- function() {
  lines <- readLines("/proc/cpuinfo")
  mapping = list()
  current = list()

  for (l in lines) {
    l <- tolower(str_strip(l))
    if (!nchar(l)) {
      if ("physical id" %in% names(current) &&
          "cpu cores" %in% names(current)) {
        mapping[[ current[["physical id"]] ]] <- current[["cpu cores"]]
      }
      current <- list()

    } else {
      if (str_starts_with(l, "physical id") ||
          str_starts_with(l, "cpu cores")) {
        kv <- strsplit(l, "\\t+:")[[1]]
        current[[ kv[[1]] ]] <- kv[[2]]
      }
    }
  }

  sum(as.integer(unlist(mapping)))
}
