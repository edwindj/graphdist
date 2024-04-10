N <- 1e6
M <- 1e8

x <- sample(M, size = N, replace = TRUE) |> as.character()

system.time({
  l <- rcpp_factorize(x)
})

system.time({
  l <- rcpp_factorize2(x)
})


system.time({
  l <- factor(x)
})

fast_factorize <- function(x, levs = NULL){
  lvls <- union(levs, x)
  l <- match(x, lvls)
  class(l) <- "factor"
  levels(l) <- lvls
  l
}

system.time({
  lvls <- union(x, x)
  l <- match(x, lvls)
  class(l) <- "factor"
  levels(l) <- lvls
})


l


system.time({
  unique(c(x,x))
})

system.time({
  union(x,x)
})
