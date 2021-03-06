
#include <Rcpp.h>
#include "hashfunc.h"
using namespace Rcpp;


int ZobristHash(IntegerVector &key, std::vector<unsigned int> &randomint)
{
  int out = 0;
  for (size_t j = 0; j < key.size(); j++) out ^= randomint[key[j]-1];

  return out;
}


IntegerVector ZobristHash_vec(std::vector< std::vector<int> > &keys,
                               std::vector<unsigned int> &randomint)
{
  int n = keys.size();
  IntegerVector out(n);

  for (int i = 0; i < n; i++)
  {
    out[i] = 0;
    for (size_t j = 0; j < keys[i].size(); j++)
      out[i] ^= randomint[keys[i][j]-1];
  }

  return out;
}



/*** R

*/
