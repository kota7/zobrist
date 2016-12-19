#ifndef HASHMETHODHEADERDEF
#define HASHMETHODHEADERDEF

#include <Rcpp.h>
#include <unordered_map>
#include "hashfunc.h"
#include "keyStrConversion.h"

using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector LocateKey(IntegerVector &key, int keysize,
                        std::vector<unsigned int> &randomint,
                        List &hashtable);


// [[Rcpp::export]]
IntegerMatrix LocateKeys(ListOf<IntegerVector> &keys,
                         int keysize,
                         std::vector<unsigned int> &randomint,
                         List &hashtable);


// [[Rcpp::export]]
List GetValueByKey(IntegerVector &key, int keysize,
                   std::vector<unsigned int> &randomint,
                   List &hashtable);


// [[Rcpp::export]]
List GetValueByKeys(ListOf<IntegerVector> &keys,
                    int keysize,
                    std::vector<unsigned int> &randomint,
                    List &hashtable);

// [[Rcpp::export]]
bool FindKey(IntegerVector &key, int keysize,
             std::vector<unsigned int> &randomint,
             List &hashtable);

// [[Rcpp::export]]
LogicalVector FindKeys(ListOf<IntegerVector> &keys, int keysize,
                       std::vector<unsigned int> &randomint,
                       List &hashtable);

#endif
