library(data.table)
library(graphdist)
# big example
set.seed(1)
N <- 1e4
M <- 1e6

edges <- data.table(
  from = sample(N, size=M, replace = TRUE),
  to   = sample(N, size=M, replace = TRUE),
  value = 1
)[from != to,] |> setDF()

from <- sample(N, size = 1e3)
member <- (seq_len(N) %in% from)

library(spam)
library(spam64)
options(spam.force64 = TRUE)
E <- spam(list(i = edges$from, j = edges$to, x = edges$value), nrow=N, ncol=N)
# saveRDS(E, "E.rds")

max_d <- 4
# E <- readRDS("E.rds")
l <- graphdist:::rcpp_member_distance2(E, member, from, max_d=max_d)

system.time({
  r <- get_member_distance_spam(E, member = member, from = from, max_d = max_d, chunksize = 1e2, ncores = 1)
  })


