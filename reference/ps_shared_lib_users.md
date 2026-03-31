# List all processes that loaded a shared library

List all processes that loaded a shared library

## Usage

``` r
ps_shared_lib_users(paths, user = ps_username(), filter = NULL)
```

## Arguments

- paths:

  Character vector of paths of shared libraries to look up. They must be
  absolute paths. They don't need to exist. Forward slashes are
  converted to backward slashes on Windows, and the output will always
  have backward slashes in the paths.

- user:

  Character scalar or `NULL`. If not `NULL`, then only the processes of
  this user are considered. It defaults to the current user.

- filter:

  Character vector or `NULL`. If not NULL, then it is a vector of glob
  expressions, used to filter the process names.

## Value

A data frame with columns:

- `dll`: the file name of the dll file, without the path,

- `path`: path to the shared library,

- `pid`: process ID of the process,

- `name`: name of the process,

- `username`: username of process owner,

- `ps_handle`: `ps_handle` object, that can be used to further query and
  manipulate the process.

## Details

### Notes:

This function currently only works on Windows.

On Windows, a 32 bit R process can only list other 32 bit processes.
Similarly, a 64 bit R process can only list other 64 bit processes. This
is a limitation of the Windows API.

Even though Windows file systems are (almost always) case insensitive,
the matching of `paths`, `user` and also `filter` are case sensitive.
This might change in the future.

This function can be very slow on Windows, because it needs to enumerate
all shared libraries of all processes in the system, unless the `filter`
argument is set. Make sure you set `filter` if you can.

If you want to look up multiple shared libraries, list all of them in
`paths`, instead of calling `ps_shared_lib_users` for each individually.

If you are after libraries loaded by R processes, you might want to set
`filter` to `c("Rgui.exe", "Rterm.exe", "rsession.exe")` The last one is
for RStudio.

## See also

Other shared library tools:
[`ps_shared_libs()`](https://ps.r-lib.org/reference/ps_shared_libs.md)

## Examples

``` r
if (FALSE) { # ps::ps_is_supported() && !ps:::is_cran_check() && ps::ps_os_type()[["WINDOWS"]]
dlls <- vapply(getLoadedDLLs(), "[[", character(1), "path")
psdll <- dlls[["ps"]][[1]]
r_procs <- c("Rgui.exe", "Rterm.exe", "rsession.exe")
ps_shared_lib_users(psdll, filter = r_procs)
}
```
