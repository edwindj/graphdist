#include "../inst/include/graphdist_types.h"

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;
using namespace Rcpp;

struct PerNode : public Worker
{
  // source matrix
  RcppSparse::Matrix& input;
  const RVector<int> nodes;
  int max_d;

  // destination matrix
  RMatrix<double> counts;

  // initialize with source and destination
  PerNode( RcppSparse::Matrix& input
         , IntegerVector nodes
         , NumericMatrix output
         , int max_d = 5
         )
    : input(input), nodes(nodes), counts(output), max_d(max_d) {
  }

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t node = begin; node < end; node++){
      auto l = get_distsparse(input, node, max_d);
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_par(RcppSparse::Matrix mat, IntegerVector nodes, int max_d = 5){
  NumericMatrix output(max_d, nodes.length());

  PerNode per_node(mat, nodes, output, max_d);

  parallelFor(0, mat.ncol(), per_node);
  return output;
}

/*** R
library(microbenchmark)
# microbenchmark( R = sqrt(M)
#               , parallel = rcpp_par(M)
#               )
*/


