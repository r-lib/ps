
decorator <- function(deco, fun) {
  clos <- function() {
    stop("Need to call decorate() on this object first")
  }
  attr(clos, "deco") <- deco
  attr(clos, "fun") <- fun
  class(clos) <- c("decorator", class(clos))
  clos
}

 decorate <- function(x) {
  method_names <- ls(x)
  for (mnm in method_names) {
    if (inherits(x[[mnm]], "decorator")) {
      unlockBinding(mnm, x)
      env <- environment(x[[mnm]])
      deco <- attr(x[[mnm]], "deco")
      fun <- attr(x[[mnm]], "fun")
      environment(fun) <- env
      x[[mnm]] <- deco(fun)
      parent.env(environment(x[[mnm]])) <- env
      lockBinding(mnm, x)
    }
  }
  x
}
