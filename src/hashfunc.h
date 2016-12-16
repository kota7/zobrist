#ifndef HASHFUNCHEADERDEF
#define HASHFUNCHEADERDEF


#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
int hashfunc(IntegerVector &key, std::vector<unsigned int> &randomint);


// [[Rcpp::export]]
IntegerVector hashfunc_vec_cpp(std::vector< std::vector<int> > &keys,
                               std::vector<unsigned int> &randomint);

#endif
