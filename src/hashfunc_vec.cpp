#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector hashfunc_vec_cpp(std::vector< std::vector<int> > keys,
                               IntegerVector randomint)
{
  long long n = keys.size();
  IntegerVector out(n);

  for (long long i = 0; i < n; i++)
  {
    out[i] = 0;
    for (size_t j = 0; j < keys[i].size(); j++)
    {
      out[i] ^= randomint[keys[i][j]-1];
    }
  }

  return out;
}



/*** R

*/
