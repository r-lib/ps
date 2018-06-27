
decorator <- function(deco, ...) {
  args <- list(...)
  fun <- tail(args, 1)[[1]]
  deco <- c(list(deco), head(args, -1))
  clos <- function() {
    stop("Need to call decorate() on this object first")
  }
  attr(clos, "deco") <- deco
  attr(clos, "fun") <- fun
  class(clos) <- c("decorator", class(clos))
  clos
}

decorate <- function(x) {

  decorate_method <- function(deco, fun, env) {
    environment(fun) <- env
    meth <- deco(fun)
    parent.env(environment(meth)) <- env
    meth
  }

  method_names <- ls(x)
  for (mnm in method_names) {
    if (inherits(x[[mnm]], "decorator")) {
      get("unlockBinding", baseenv())(mnm, x)
      deco <- attr(x[[mnm]], "deco")[[1]]
      fun <- attr(x[[mnm]], "fun")
      env <- environment(x[[mnm]])
      x[[mnm]] <- decorate_method(deco, fun, env)
      lockBinding(mnm, x)
    }
  }
  x
}
