#ifndef KEYSTRCONVHEADERDEF
#define KEYSTRCONVHEADERDEF

#include <Rcpp.h>
#include <string>
#include <map>
using namespace Rcpp;

// Naming rule of the hash table entries


// look up tables bin -> hex, and hex -> bin
char bin_to_hex(const std::string bin);
std::string hex_to_bin(const char hex);


// [[Rcpp::export]]
std::string KeyToStr(IntegerVector x, int keysize);


// [[Rcpp::export]]
IntegerVector StrToKey(std::string x, int keysize);


// vectorized versions
// [[Rcpp::export]]
CharacterVector KeysToStrs(ListOf<IntegerVector> x, int keysize);

// [[Rcpp::export]]
ListOf<IntegerVector> StrsToKeys(CharacterVector x, int keysize);


#endif
