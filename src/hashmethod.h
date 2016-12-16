#ifndef HASHMETHODHEADERDEF
#define HASHMETHODHEADERDEF

#include <Rcpp.h>
#include "hashfunc.h"
#include "keyStrConversion.h"

using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector LocateKey(IntegerVector &key, int keysize,
                        IntegerVector &randomint,
                        ListOf<List> &hashtable);


// [[Rcpp::export]]
IntegerMatrix LocateKeys(ListOf<IntegerVector> &keys,
                         int keysize,
                         IntegerVector &randomint,
                         ListOf<List> &hashtable);


// [[Rcpp::export]]
List GetValueByKey(IntegerVector &key, int keysize,
                   IntegerVector &randomint,
                   ListOf<List> &hashtable);


// [[Rcpp::export]]
List GetValueByKeys(ListOf<IntegerVector> &keys,
                    int keysize,
                    IntegerVector &randomint,
                    ListOf<List> &hashtable);

// [[Rcpp::export]]
bool FindKey(IntegerVector &key, int keysize,
             IntegerVector &randomint,
             ListOf<List> &hashtable);

// [[Rcpp::export]]
LogicalVector FindKeys(ListOf<IntegerVector> &keys, int keysize,
                       IntegerVector &randomint,
                       ListOf<List> &hashtable);

#endif