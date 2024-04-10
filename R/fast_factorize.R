# very fast factorizing for large vectors
#
# makes it easy to create a factor that is consistent with the levels
# provided by levs. Note that new levels will be added.
fast_factorize <- function(x, levs = NULL){
  lvls <- union(levs, x) # same as unique(c(levs,x))
  l <- match(x, lvls)
  class(l) <- "factor"
  levels(l) <- lvls
  l
}
