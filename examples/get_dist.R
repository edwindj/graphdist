d <-
"from,to
A,B
A,C
B,D
D,E
" |> data.table::fread(colClasses = "factor")

library(Matrix)
levs <- c(levels(d$from), levels(d$to)) |> unique()
N <- length(levs)

d[, from := factor(from, levels = levs)]
d[, to := factor(to, levels=levs)]

mat <- sparseMatrix(j = d$from, i = d$to, x = 1, dims = c(N,N), dimnames = list(from=levs, to=levs))
member <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
get_distsparse_cpp(mat, member, 1, 3)

# ds <- data.frame(
#   name = levs,
#   d = NA_integer_
# )
#
# f <- 1
# ds$d[f] <- 0
#
set.seed(1)
N <- 1e6
M <- 1e8
from <- sample(N, size=M, replace = TRUE)
to <- sample(N, size=M, replace = TRUE)
mat <- sparseMatrix(j = from, i = to, x = 1, dims=c(N,N))
member <- (runif(N) > 0.99)

system.time({
  r <- get_distsparse_cpp(mat, member, 1,  5)
})

hist(r$distance)

system.time({
  r <- rcpp_node_count_dist(mat, member, 1:5,  4)
})


r$nodes_at_d |> sum()

r#
# x <- data.frame(from, to)
# x <- x[x$from != x$to,]
#
# system.time({
#   get_dist_cpp(x, from[1], 2, N)
# })
#
# library(Matrix)
#
# get_distsparse_cpp()
#
#
