#include "../inst/include/graphdist_types.h"
using namespace Rcpp;
//[[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
List rcpp_get_dist_sparse( RcppSparse::Matrix& mat
                         , LogicalVector& member // same dimension as row / col mat
                         , std::size_t node // node idx
                         , int max_d
                        ){
  node -= 1; // R is 1-based, C++ 0-based

  // assumption: from, to and member have same domain, should be taken care of
  // and checked in R code.
  int max_n = member.length();

  IntegerVector distance(max_n, R_NaInt);

  IntegerVector nodes_at_d(max_d);
  IntegerVector members_at_d(max_d);

  std::vector<int> current;
  current.push_back(node);
  distance[node] = 0;

  // maybe a better iterator
  int dc = 0;

  while (dc < max_d && current.size() > 0){
    int n_at_d = 0;
    int n_members = 0;
    std::vector<int> next;
    for (auto from = current.begin(); from != current.end(); from++){
      auto rows = mat.InnerIndices(*from);
      for (auto i = rows.begin(); i != rows.end(); i++){
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
List rcpp_member_distance( RcppSparse::Matrix& mat
                                  , LogicalVector& member
                                  , IntegerVector from
                                  , int max_d
                                  ){
  int ncols = from.length();
  IntegerMatrix n_nodes(max_d,ncols);
  IntegerMatrix n_members(max_d,ncols);
  for (int i = 0; i < from.length(); i++){
    auto node_id = from[i];
    auto res = rcpp_get_dist_sparse(mat, member, node_id, max_d);
    n_nodes(_, i) = as<IntegerVector>(res["nodes_at_d"]);
    n_members(_, i) = as<IntegerVector>(res["members_at_d"]);
  }

  return List::create(
    _["n_nodes"] = n_nodes,
    _["n_members"] = n_members
  );
}

/*** R
set.seed(1)
library(Matrix)
mat <- rsparsematrix(1e4, 1e4, 0.02)
member <- (runif(1e4) > 0.9) # i.e. 10% chance of being a member

system.time({
  r <- rcpp_get_dist_sparse(mat, member, 2, max_d=5)
})

hist(r$distance)

system.time({
  m <- rcpp_member_distance(mat, member, seq_len(10), max_d=5)
})

(m$n_members/m$n_nodes) |> round(2)

*/
