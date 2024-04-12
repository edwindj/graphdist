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

# r <- get_member_distance(d, from = c("A", "D"), max_distance = 2)
# print(r)

library(data.table)
# big example
set.seed(1)
N <- 1e4
M <- 1e6

edges <- data.table(
  from = sample(N, size=M, replace = TRUE),
  to   = sample(N, size=M, replace = TRUE),
  value = 1
)[from != to,] |> setDF()

from <- sample(N, size = 1e2)
member <- (seq_len(N) %in% from)

library(spam)
library(spam64)
options(spam.force64 = TRUE)
E <- spam(list(i = edges$from, j = edges$to, x = edges$value), nrow=N, ncol=N)
saveRDS(E, "E.rds")

E <- readRDS("E.rds")
system.time({
  r <- rcpp_member_distance2(E, member = member, from = from, max_d = 3)
})
r

system.time({
  r <- rcpp_member_distance_par(E, member = member, from = as.character(from), max_d = 3)
})
r
