library(data.table)

d <-
  "from,to
A,B
A,C
B,D
D,E
" |> data.table::fread(colClasses = "factor")

# assume data.frame with to and from
setDF(d)

r <- get_member_distance(d, from = c("A", "D"), max_distance = 2)
print(r)

library(data.table)
# big example
set.seed(1)
N <- 1e6
M <- 1e8

edges <- data.table(
  from = sample(N, size=M, replace = TRUE),
  to   = sample(N, size=M, replace = TRUE)
)[from != to,] |> setDF()

from <- sample(N, size = 1e2)

E <- to_sparse_matrix(edges)

saveRDS(E, "E.rds")

E <- readRDS("E.rds")
system.time({
  r <- get_member_distance(E, from = from, max_distance = 3)
})
r
