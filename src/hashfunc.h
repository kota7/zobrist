#ifndef HASHFUNCHEADERDEF
#define HASHFUNCHEADERDEF


#include <Rcpp.h>
using namespace Rcpp;


// use std::vector<unsigned int> instead of IntegerVector since
// the former is up to 32 bit, while the latter 31
// by using unsigned, we can have hash size 32
//
// the cost is type conversion when calling the function, which is
// negligible since the vector size is relatively small
// (typical games have up to thousand bit for state)

// [[Rcpp::export]]
int ZobristHash(IntegerVector &key, std::vector<unsigned int> &randomint);


// [[Rcpp::export]]
IntegerVector ZobristHash_vec(std::vector< std::vector<int> > &keys,
                              std::vector<unsigned int> &randomint);

#endif
