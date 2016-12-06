// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// hashfunc_vec_cpp
IntegerVector hashfunc_vec_cpp(std::vector< std::vector<int> > keys, IntegerVector randomint);
RcppExport SEXP zobrist_hashfunc_vec_cpp(SEXP keysSEXP, SEXP randomintSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< std::vector<int> > >::type keys(keysSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type randomint(randomintSEXP);
    rcpp_result_gen = Rcpp::wrap(hashfunc_vec_cpp(keys, randomint));
    return rcpp_result_gen;
END_RCPP
}
