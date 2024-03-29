# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

get_dist_cpp <- function(x, node, d, max_n) {
    .Call(`_graphdist_get_dist`, x, node, d, max_n)
}

get_distsparse_cpp <- function(mat, node, d) {
    .Call(`_graphdist_get_distsparse`, mat, node, d)
}

Rcpp_colSums <- function(mat) {
    .Call(`_graphdist_Rcpp_colSums`, mat)
}

rcpp_par <- function(mat, nodes, max_d = 5L) {
    .Call(`_graphdist_rcpp_par`, mat, nodes, max_d)
}

