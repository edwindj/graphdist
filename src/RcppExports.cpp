// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/graphdist_types.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_get_dist
List rcpp_get_dist(DataFrame x, int node, int d, int max_n);
RcppExport SEXP _graphdist_rcpp_get_dist(SEXP xSEXP, SEXP nodeSEXP, SEXP dSEXP, SEXP max_nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< int >::type d(dSEXP);
    Rcpp::traits::input_parameter< int >::type max_n(max_nSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_dist(x, node, d, max_n));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_dist_sparse
List rcpp_get_dist_sparse(RcppSparse::Matrix& mat, LogicalVector& member, std::size_t node, int max_d);
RcppExport SEXP _graphdist_rcpp_get_dist_sparse(SEXP matSEXP, SEXP memberSEXP, SEXP nodeSEXP, SEXP max_dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RcppSparse::Matrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< LogicalVector& >::type member(memberSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< int >::type max_d(max_dSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_dist_sparse(mat, member, node, max_d));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_member_distance
List rcpp_member_distance(RcppSparse::Matrix& mat, LogicalVector& member, IntegerVector from, int max_d);
RcppExport SEXP _graphdist_rcpp_member_distance(SEXP matSEXP, SEXP memberSEXP, SEXP fromSEXP, SEXP max_dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RcppSparse::Matrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< LogicalVector& >::type member(memberSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type from(fromSEXP);
    Rcpp::traits::input_parameter< int >::type max_d(max_dSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_member_distance(mat, member, from, max_d));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_get_dist_sparse2
List rcpp_get_dist_sparse2(RcppSpam::Matrix& mat, LogicalVector& member, R_xlen_t node, int max_d);
RcppExport SEXP _graphdist_rcpp_get_dist_sparse2(SEXP matSEXP, SEXP memberSEXP, SEXP nodeSEXP, SEXP max_dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RcppSpam::Matrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< LogicalVector& >::type member(memberSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type node(nodeSEXP);
    Rcpp::traits::input_parameter< int >::type max_d(max_dSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_get_dist_sparse2(mat, member, node, max_d));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_member_distance2
List rcpp_member_distance2(RcppSpam::Matrix& mat, LogicalVector& member, NumericVector from, int max_d);
RcppExport SEXP _graphdist_rcpp_member_distance2(SEXP matSEXP, SEXP memberSEXP, SEXP fromSEXP, SEXP max_dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< RcppSpam::Matrix& >::type mat(matSEXP);
    Rcpp::traits::input_parameter< LogicalVector& >::type member(memberSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type from(fromSEXP);
    Rcpp::traits::input_parameter< int >::type max_d(max_dSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_member_distance2(mat, member, from, max_d));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_to_spam_sorted
S4 rcpp_to_spam_sorted(NumericVector& from, NumericVector& to, int N);
RcppExport SEXP _graphdist_rcpp_to_spam_sorted(SEXP fromSEXP, SEXP toSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type from(fromSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type to(toSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_to_spam_sorted(from, to, N));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_to_spam
S4 rcpp_to_spam(NumericVector& from, NumericVector& to, int N);
RcppExport SEXP _graphdist_rcpp_to_spam(SEXP fromSEXP, SEXP toSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type from(fromSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type to(toSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_to_spam(from, to, N));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_graphdist_rcpp_get_dist", (DL_FUNC) &_graphdist_rcpp_get_dist, 4},
    {"_graphdist_rcpp_get_dist_sparse", (DL_FUNC) &_graphdist_rcpp_get_dist_sparse, 4},
    {"_graphdist_rcpp_member_distance", (DL_FUNC) &_graphdist_rcpp_member_distance, 4},
    {"_graphdist_rcpp_get_dist_sparse2", (DL_FUNC) &_graphdist_rcpp_get_dist_sparse2, 4},
    {"_graphdist_rcpp_member_distance2", (DL_FUNC) &_graphdist_rcpp_member_distance2, 4},
    {"_graphdist_rcpp_to_spam_sorted", (DL_FUNC) &_graphdist_rcpp_to_spam_sorted, 3},
    {"_graphdist_rcpp_to_spam", (DL_FUNC) &_graphdist_rcpp_to_spam, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_graphdist(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
