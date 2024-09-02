
#' List currently running applications
#'
#' This function currently only works on macOS.
#'
#' @return A data frame with columns:
#'   - `pid`: integer process id.
#'   - `name`: process name.
#'   - `bundle_identifier`: bundle identifier, e.g. `com.apple.dock`.
#'   - `bundle_url`: bundle URL, a `file://` URL to the app bundle.
#'   - `arch`: executable architecture, possible values are
#'     `r paste(macos_archs$name, collapse = ", ")`.
#'   - `executable_url`: `file://` URL to the executable file.
#'   - `launch_date`: launch time stamp, a `POSIXct` object, may be `NA`.
#'   - `finished_launching`: whether the app has finished launching.
#'   - `active`: whether the app is active.
#'   - `activation_policy`: one of the following values:
#'     * `regular`: the application is an ordinary app that appears in the
#'       Dock and may have a user interface.
#'     * `accessory`: the application doesn’t appear in the Dock and
#'       doesn’t have a menu bar, but it may be activated programmatically
#'       or by clicking on one of its windows.
#'     * `prohibited`: the application doesn’t appear in the Dock and may
#'       not create windows or be activated.
#'
#' @export
#' @examplesIf ps_is_supported() && ps_os_type()[["MACOS"]] && !ps:::is_cran_check()
#' ps_apps()

ps_apps <- function() {
  if (!ps_os_type()[["MACOS"]]) {
    stop("'ps_apps()' is only implemented on macOS")
  }
  tab <- as_data_frame(.Call(ps__list_apps))

  tab <- tab[, c("pid", setdiff(names(tab), "pid"))]
  tab[["arch"]] <- macos_archs$name[match(tab[["arch"]], macos_archs$code)]
  tab[["launch_date"]] <- parse_iso_8601(
    sub(" +", "+", fixed = TRUE, tab[["launch_date"]])
  )
  # drop the ones without a pid, they are clearly (?) not running
  tab <- tab[tab$pid != -1, ]
  tab
}

macos_archs <- data.frame(
  name = c("arm64",    "i386",     "x86_64",   "ppc",      "ppc64"),
  code = c(0x0100000c, 0x00000007, 0x01000007, 0x00000012, 0x01000012)
)
