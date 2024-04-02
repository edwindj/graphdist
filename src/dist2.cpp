#include "../inst/include/graphdist_types.h"
using namespace Rcpp;

// [[Rcpp::export("get_distsparse_cpp")]]
List get_distsparse(RcppSparse::Matrix& mat, std::size_t node, int d){
  // assumption: from and to have same domain
  int max_n = mat.cols();
  IntegerVector distance(max_n, R_NaInt);
  IntegerVector nodes_at_d(d+1);

  std::vector<int> current;
  current.push_back(node);
  distance[node] = 0;

  // maybe a better iterator
  int dc = 1;
  while (dc <= d && current.size() > 0){
    int n_at_d = 0;
    std::vector<int> next;
    for (auto from = current.begin(); from != current.end(); from++){
      auto rows = mat.InnerIndices(*from);
      for (auto i = rows.begin(); i != rows.end(); i++){
        int to = *i;
        if (IntegerVector::is_na(distance[to])){
          n_at_d += 1;
          next.push_back(to);
          distance[to] = dc;
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
NumericVector Rcpp_colSums(RcppSparse::Matrix& mat){
  return mat.colSums();
}


/*** R
library(Matrix)
mat <- rsparsematrix(1e4, 1e4, 0.05)
# colSums <- Rcpp_colSums(mat)
system.time({
  r <- get_distsparse_cpp(mat, 2, d=5)
})

rcpp_par(mat, 1)

hist(r$distance)

*/
