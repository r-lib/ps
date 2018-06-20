
common_puids <- function(values) {
  names(values) <- c("real", "effective", "saved")
  values
}

common_pcputimes <- function(values) {
  names(values) <-
    c("user", "system", "children_user", "children_system")
  values
}
