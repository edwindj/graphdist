// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/graphdist_types.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// get_dist
List get_dist(DataFrame x, int node, int d, int max_n);
RcppExport SEXP _graphdist_get_dist(SEXP xSEXP, SEXP nodeSEXP, SEXP dSEXP, SEXP max_nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< int >::type d(dSEXP);
    Rcpp::traits::input_parameter< int >::type max_n(max_nSEXP);
    rcpp_result_gen = Rcpp::wrap(get_dist(x, node, d, max_n));
    return rcpp_result_gen;
END_RCPP
}
// get_distsparse
List get_distsparse(RcppSparse::Matrix& mat, std::size_t node, int d);
RcppExport SEXP _graphdist_get_distsparse(SEXP matSEXP, SEXP nodeSEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RcppSparse::Matrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< int >::type d(dSEXP);
    rcpp_result_gen = Rcpp::wrap(get_distsparse(mat, node, d));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_colSums
NumericVector Rcpp_colSums(RcppSparse::Matrix& mat);
RcppExport SEXP _graphdist_Rcpp_colSums(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RcppSparse::Matrix& >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_colSums(mat));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_par
NumericMatrix rcpp_par(RcppSparse::Matrix mat, IntegerVector nodes, int max_d);
RcppExport SEXP _graphdist_rcpp_par(SEXP matSEXP, SEXP nodesSEXP, SEXP max_dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RcppSparse::Matrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type nodes(nodesSEXP);
    Rcpp::traits::input_parameter< int >::type max_d(max_dSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_par(mat, nodes, max_d));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_graphdist_get_dist", (DL_FUNC) &_graphdist_get_dist, 4},
    {"_graphdist_get_distsparse", (DL_FUNC) &_graphdist_get_distsparse, 3},
    {"_graphdist_Rcpp_colSums", (DL_FUNC) &_graphdist_Rcpp_colSums, 1},
    {"_graphdist_rcpp_par", (DL_FUNC) &_graphdist_rcpp_par, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_graphdist(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
