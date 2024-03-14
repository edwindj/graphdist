#include <Rcpp.h>
#include "graphdist_types.h"
// using namespace Rcpp;

// [[Rcpp::export("get_distsparse_cpp")]]
Rcpp::List get_distsparse(Rcpp::dgCMatrix& mat, int node, int d){
  // assumption: from and to have same domain
  int max_n = mat.cols();
  Rcpp::IntegerVector distance(max_n, R_NaInt);
  Rcpp::IntegerVector nodes_at_d(d+1);

  std::vector<int> current;
  current.push_back(node);

  // maybe a better iterator
  int dc = 1;
  while (dc <= d && current.size() > 0){
    int n_at_d = 0;
    std::vector<int> next;
    for (auto from = current.begin(); from != current.end(); from++){
      for (auto i = mat.begin_col(*from); i != mat.end_col(*from); i++){
        int to = i.row();
        if (Rcpp::IntegerVector::is_na(distance[to])){
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

  return Rcpp::List::create(
    Rcpp::_["node"] = node,
    Rcpp::_["d"] = d,
    Rcpp::_["dc"] = dc,
    Rcpp::_["distance"] = distance,
    Rcpp::_["nodes_at_d"] = nodes_at_d
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector Rcpp_colSums(Rcpp::dgCMatrix& mat){
  return mat.colSums();
}


/*** R
library(Matrix)
mat <- rsparsematrix(1e4, 1e4, 0.05)
# colSums <- Rcpp_colSums(mat)
system.time({
  r <- get_distsparse_cpp(mat, 1, d=5)
})

hist(r$distance)
*/
