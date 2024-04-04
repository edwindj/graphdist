#' Assess for a set of nodes how separated they are in a large network.
#'
#' This method can be used to find how many neighbors there are at distance d
#' for each node in `from`. Furthermore it also return the number of neighbors that
#' are part of the set `from`.
#' @export
#' @param edges `data.frame` with a `$from` and `$to` column with node ids.
#' @param from `character` list of node ids that are considered a member
#' @param max_distance `integer` maximum distance at which the calculation stops.
get_member_distance <- function( edges
                               , from
                               , max_distance = 3
                               ){
  E <- to_sparse_matrix(edges)

  member <- colnames(E) %in% from
  f <- which(member)

  res <- rcpp_member_distance( E
                             , member = member
                             , from = f
                             , max_d = max_distance
                             )

  dmn <- list( from = from
             , distance = seq_len(max_distance)
             )

  n_nodes <- t(res$n_nodes)
  dimnames(n_nodes) <- dmn

  n_members <- t(res$n_members)
  dimnames(n_members) <- dmn


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
