
memoize <- function(fun) {
  fun
  cache <- NULL
  dec <- function(...) {
    if (length(list(...))) return(fun(...))
    if (is.null(cache)) cache <<- fun(...)
    cache
  }
  attr(dec, "clear") <- function() cache <<- TRUE
  class(dec) <- c("memoize", class(dec))
  dec
}

`$.memoize`  <- function(x, name) {
  switch(
    name,
    "clear" = attr(x, "clear")(),
    stop("unknown memoize method")
  )
}

memoize_when_activated <- function(fun) {
  fun
  cache <- NULL
  active <- FALSE
  dec <- function(...) {
    ## No cache if not active or function has arguments
    if (!active || length(list(...))) return(fun(...))
    if (is.null(cache)) cache <<- fun(...)
    cache
  }
  attr(dec, "activate") <- function() active <<- TRUE
  attr(dec, "deactivate") <- function() { active <<- FALSE; cache <<- NULL }
  class(dec) <- c("memoize_when_activated", class(dec))
  dec
}

`$.memoize_when_activated` <- function(x, name) {
  switch(
    name,
    "activate" = attr(x, "activate")(),
    "deactivate" = attr(x, "deactivate")(),
    stop("unknown memoize method")
  )
}
