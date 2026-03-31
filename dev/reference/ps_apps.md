# List currently running applications

This function currently only works on macOS.

## Usage

``` r
ps_apps()
```

## Value

A data frame with columns:

- `pid`: integer process id.

- `name`: process name.

- `bundle_identifier`: bundle identifier, e.g. `com.apple.dock`.

- `bundle_url`: bundle URL, a `file://` URL to the app bundle.

- `arch`: executable architecture, possible values are arm64, i386,
  x86_64, ppc, ppc64.

- `executable_url`: `file://` URL to the executable file.

- `launch_date`: launch time stamp, a `POSIXct` object, may be `NA`.

- `finished_launching`: whether the app has finished launching.

- `active`: whether the app is active.

- `activation_policy`: one of the following values:

  - `regular`: the application is an ordinary app that appears in the
    Dock and may have a user interface.

  - `accessory`: the application doesn’t appear in the Dock and doesn’t
    have a menu bar, but it may be activated programmatically or by
    clicking on one of its windows.

  - `prohibited`: the application doesn’t appear in the Dock and may not
    create windows or be activated.

## Examples

``` r
if (FALSE) { # ps_is_supported() && ps_os_type()[["MACOS"]] && !ps:::is_cran_check()
ps_apps()
}
```
