#include "../inst/include/RcppSpam.h"
using namespace Rcpp;



// [[Rcpp::export]]
List rcpp_get_dist_sparse2( RcppSpam::Matrix& mat
                          , LogicalVector& member // same dimension as row / col mat
                          , R_xlen_t node // node idx
                          , int max_d
                          ){
  node -= 1; // R is 1-based, C++ 0-based

  // assumption: from, to and member have same domain, should be taken care of
  // and checked in R code.
  R_xlen_t max_n = member.length();

  IntegerVector distance(max_n, R_NaInt);

  IntegerVector nodes_at_d(max_d);
  IntegerVector members_at_d(max_d);

  std::vector<R_xlen_t> current;
  current.push_back(node);
  distance[node] = 0;

  // maybe a better iterator
  int dc = 0;

  while (dc < max_d && current.size() > 0){
    int n_at_d = 0;
    int n_members = 0;
    std::vector<R_xlen_t> next;
    for (auto from = current.begin(); from != current.end(); from++){
      auto cols = mat.InnerIndices(*from);
      for (auto i = cols.begin(); i != cols.end(); i++){
        int to = *i;
        if (IntegerVector::is_na(distance[to])){
          n_at_d += 1;
          n_members += member[to];
          next.push_back(to);
          distance[to] = dc+1;
        }
      }
    }
    nodes_at_d[dc] = n_at_d;
    members_at_d[dc] = n_members;
    dc += 1;
    current = next;
  }

  return List::create(
    _["node"] = node,
    _["max_d"] = max_d,
    _["dc"] = dc,
    _["distance"] = distance,
    _["nodes_at_d"] = nodes_at_d,
    _["members_at_d"] = members_at_d
  );
}

// [[Rcpp::export]]
List rcpp_member_distance2( RcppSpam::Matrix& mat
                                  , LogicalVector& member
                                  , NumericVector from
                                  , int max_d
                                  ){
  int ncols = from.length();
  IntegerMatrix n_nodes(max_d,ncols);
  IntegerMatrix n_members(max_d,ncols);
  for (R_xlen_t i = 0; i < from.length(); i++){
    auto node_id = from[i];
    auto res = rcpp_get_dist_sparse2(mat, member, node_id, max_d);
    n_nodes(_, i) = as<IntegerVector>(res["nodes_at_d"]);
    n_members(_, i) = as<IntegerVector>(res["members_at_d"]);
  }

  return List::create(
    _["n_nodes"] = n_nodes,
    _["n_members"] = n_members
  );
}

struct compare {
  std::vector<double>& o;
  compare(std::vector<double>& o) : o(o) {};
  bool operator() (int i, int j) {return i < j;}
};

// [[Rcpp::export]]
S4 rcpp_to_spam_sorted(NumericVector& from, NumericVector& to, int N){
  // assumption from and to should have same size
  R_xlen_t M = from.size(); // number of edges

  IntegerVector dimension = {N,N};

  Rcout << "\nCreating vectors";
  NumericVector entries(M, 1);
  NumericVector colindices = to;
  NumericVector rowpointers(N+1);

  int r = 0;
  rowpointers(r++) = 1;

  Rcout << "\nFilling them...";
  for (R_xlen_t j = 0; j < M; ++j){
    while (r < from[j] && r < rowpointers.length()){
      rowpointers[r++] = j+1;
    }
  }

  while (r < rowpointers.length()){
    rowpointers[r++] = M+1;
  }

  RcppSpam::Matrix m(entries, colindices, rowpointers, dimension);
  return m.wrap();
}


// [[Rcpp::export]]
S4 rcpp_to_spam(NumericVector& from, NumericVector& to, int N){
  // assumption from and to should have same size
  R_xlen_t M = from.size(); // number of edges

  Rcout << "Calculating indices with size ";
  Rcout << M;
  Rcout << "...";

  std::vector<R_xlen_t> indices(M);
  Rcout << "\n\tiota...";
  std::iota(indices.begin(), indices.end(), 0);
  Rcout << "\n\tsorting ...";
  std::sort(indices.begin(), indices.end(),
            [&](auto a, auto b) -> bool {
              return from[a] < from[b] && to[a] < to[b];
            });

  Rcout << "\n\tsorted\n";

  IntegerVector dimension = {N,N};

  Rcout << "\nCreating vectors";
  NumericVector entries(M), colindices(M), rowpointers(N+1);

  int r = 0;
  rowpointers(r++) = 1;

  Rcout << "\nFilling them...";
  R_xlen_t j = 0;
  for (int i : indices){
    while (r < from[i] && r < rowpointers.length()){
      rowpointers[r++] = j+1;
    }
    entries[j] = 1;
    colindices[j] = to[i];
    j++;
  }

  while (r < rowpointers.length()){
    rowpointers[r++] = M+1;
  }

  RcppSpam::Matrix m(entries, colindices, rowpointers, dimension);
  return m.wrap();
}

/*** R
set.seed(1)
library(spam)
library(spam64)
options("spam.force64" = TRUE)

N <- 1e4
M <- spam_random(nrow = N, density = 0.02)
member <- (runif(N) > 0.9) # i.e. 10% chance of being a member

system.time({
  r <- rcpp_get_dist_sparse2(M, member, 2, max_d=5)
})

hist(r$distance)

system.time({
  m <- rcpp_member_distance2(M, member, seq_len(10), max_d=5)
})

(m$n_members/m$n_nodes) |> round(2)


E <- rcpp_to_spam(c(1,1,3), c(3,2,1), N=4)
str(E)
as.matrix(E)

E <- rcpp_to_spam_sorted(c(1,1,3), c(2,3,1), N=4)
str(E)

member <- c(TRUE, FALSE, FALSE, TRUE)
rcpp_member_distance2(E, member, seq_len(4), max_d=5)
*/
