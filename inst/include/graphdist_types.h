#include "RcppSparse.h"
#include "RcppSpam.h"

Rcpp::List get_distsparse(RcppSparse::Matrix& mat, std::size_t node, int d);
