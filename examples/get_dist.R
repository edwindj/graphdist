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

get_distsparse_cpp(mat, 0, 2)

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

library(microbenchmark)

microbenchmark(
  dist = get_distsparse_cpp(mat, 0, 1)
)

system.time({
  r <- get_distsparse_cpp(mat, 0, 1)
})
hist(r$distance)
#
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
