#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List MakeHashTable(std::vector< std::vector<int> > keys,
               CharacterVector key_names,
               List values) {
  List out;
  return out;
}

/*** R
*/
