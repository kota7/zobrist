#ifndef HASHFUNCHEADERDEF
#define HASHFUNCHEADERDEF


#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
int hashfunc(IntegerVector &key, IntegerVector &randomint);


// [[Rcpp::export]]
IntegerVector hashfunc_vec_cpp(std::vector< std::vector<int> > &keys,
                               IntegerVector &randomint);

#endif
