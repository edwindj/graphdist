#include "../inst/include/graphdist_types.h"
using namespace Rcpp;

// [[Rcpp::export("get_distsparse_cpp")]]
List get_distsparse(RcppSparse::Matrix& mat, std::size_t node, int d){
  // assumption: from and to have same domain
  int max_n = mat.cols();
  IntegerVector distance(max_n, R_NaInt);
  IntegerVector nodes_at_d(d);

  std::vector<int> current;
  current.push_back(node);
  distance[node] = 0;

  // maybe a better iterator
  int dc = 0;
  while (dc < d && current.size() > 0){
    int n_at_d = 0;
    std::vector<int> next;
    for (auto from = current.begin(); from != current.end(); from++){
      auto rows = mat.InnerIndices(*from);
      for (auto i = rows.begin(); i != rows.end(); i++){
        int to = *i;
        if (IntegerVector::is_na(distance[to])){
          n_at_d += 1;
          next.push_back(to);
          distance[to] = dc+1;
        }
      }
    }
    nodes_at_d[dc] = n_at_d;
    dc += 1;
    current = next;
  }

  return List::create(
    _["node"] = node,
    _["d"] = d,
    _["dc"] = dc,
    _["distance"] = distance,
    _["nodes_at_d"] = nodes_at_d
  );
}

// [[Rcpp::export]]
IntegerMatrix Rcpp_node_count_dist(RcppSparse::Matrix& mat, IntegerVector nodes, int d){
  int ncols = nodes.length();
  IntegerMatrix m(d,ncols);
  for (int n = 0; n < nodes.length(); n++){
    auto node = nodes[n];
    auto res = get_distsparse(mat, node, d);
    IntegerVector a = res["nodes_at_d"];
    m(_, n) = a;
 //   m(_, node) = res["nodes_at_d"];
  }
  return m;
}


/*** R
library(Matrix)
mat <- rsparsematrix(1e4, 1e4, 0.02)
system.time({
  r <- get_distsparse_cpp(mat, 2, d=5)
})

hist(r$distance)

m <- Rcpp_node_count_dist(mat, 1:3, 5)
m
*/
