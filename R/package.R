
ps_env <- new.env(parent = emptyenv())

Internal <- NULL

## nocov start
.onLoad <- function(libname, pkgname) {

  ## Copy the shared lib to the session temporary directory
  copy_lib(libname, pkgname)

  ps_env$constants <- new.env(parent  = emptyenv())
  .Call(ps__init, asNamespace("ps"), ps_env$constants)
  if (!is.null(ps_env$constants$signals)) {
    ps_env$constants$signals <- as.list(ps_env$constants$signals)
  }
  if (!is.null(ps_env$constants$errno)) {
    ps_env$constants$errno <- as.list(ps_env$constants$errno)
  }
  if (!is.null(ps_env$constants$address_families)) {
    ps_env$constants$address_families <-
      as.list(ps_env$constants$address_families)
  }
  if (!is.null(ps_env$constants$socket_types)) {
    ps_env$constants$socket_types <-
      as.list(ps_env$constants$socket_types)
  }

  Internal <<- get(".Internal", asNamespace("base"))

  ps_boot_time <<- memoize(ps_boot_time)
  get_terminal_map <<- memoize(get_terminal_map)
  NA_time <<- memoize(NA_time)
}

copy_lib <- function(libname, pkgname) {
  libs <- .dynLibs()
  matchidx <- vapply(libs, "[[", character(1), "name") == pkgname
  pkglibs <- libs[matchidx]
  for (lib in pkglibs) {
    dyn.unload(lib[["path"]])
  }
  .dynLibs(libs[!(libs %in% pkglibs)])

  dir.create(tmp <- tempfile(.packageName))
  tmp <- normalizePath(tmp)
  libdir <- file.path(tmp, pkgname, "libs", .Platform$r_arch)
  dir.create(libdir, recursive = TRUE)

  ext <- .Platform$dynlib.ext

  ## This is for pkgload / devtools
  lib1 <- file.path(libname, pkgname, "src", paste0(pkgname, ext))
  if (file.exists(lib1)) file.copy(lib1, libdir)

  ## This is the proper R CMD INSTALL
  lib2 <- file.path(libname, pkgname, "libs", .Platform$r_arch,
                    paste0(pkgname, ext))
  if (file.exists(lib2)) file.copy(lib2, libdir)

  ## Plus we need these as well
  file.copy(
    file.path(libname, pkgname, c("DESCRIPTION", "NAMESPACE")),
    file.path(tmp, pkgname))

  dll <- library.dynam(pkgname, pkgname, lib.loc = tmp)

  syms <- names(getDLLRegisteredRoutines(dll)$.Call)
  routines <- getNativeSymbolInfo(syms, dll)
  ns <- asNamespace(pkgname)

  ns$.__NAMESPACE__.$DLLs[[.packageName]] <- dll
  for (n in names(routines)) ns[[paste0("c_", n)]] <- routines[[n]]

  invisible()
}

## nocov end

utils::globalVariables(c("self", "super"))
