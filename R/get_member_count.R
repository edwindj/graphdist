#' @export
get_member_count <- function(edges, from, distance = 3){
  E <- to_sparse_matrix(edges)

  member <- colnames(E) %in% from
  f <- which(member)

  res <- rcpp_node_count_dist(E, member, f, d = distance)

  n_nodes <- t(res$n_nodes)
  dimnames(n_nodes) <- list("from" = from, "distance" = seq_len(distance))

  n_members <- t(res$n_members)
  dimnames(n_members) <- list("from" = from, "distance" = seq_len(distance))


  list(
    E = E,
    n_nodes = n_nodes,
    n_members = n_members
  )
}

#' edges is data.frame
#' @importFrom data.table as.data.table
#' @importFrom Matrix sparseMatrix
to_sparse_matrix <- function(edges){
  stopifnot(all(c("to", "from") %in% names(edges)))
  d <- as.data.table(edges[, c("to", "from")])
  levs <- d[, union(from, to)]
  N <- length(levs)

  d[, from := factor(from, levels = levs)]
  d[, to := factor(to, levels=levs)]

  mat <- sparseMatrix(j = d$from, i = d$to, x = 1, dims = c(N,N), dimnames = list(from=levs, to=levs))
  mat
}
