
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
    meth <- deco(fun)
    parent.env(environment(meth)) <- env
    meth
  }

  method_names <- ls(x, all.names = TRUE)
  for (mnm in method_names) {
    if (inherits(x[[mnm]], "decorator")) {
      get("unlockBinding", baseenv())(mnm, x)
      deco <- attr(x[[mnm]], "deco")
      fun <- attr(x[[mnm]], "fun")
      env <- environment(x[[mnm]])
      environment(fun) <- env

      for (d in deco) {
        fun <- decorate_method(d, fun, env)
      }

      x[[mnm]] <- fun
      lockBinding(mnm, x)
    }
  }
  x
}
