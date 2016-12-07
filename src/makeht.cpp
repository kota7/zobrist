#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List MakeHashTable(int htsize,
                   List values,
                   IntegerVector index)
{
  // input validation
  // values and index must be the same length
  if (values.size() != index.size())
    stop("values and index must have the same length");
  // all index must be smaller than htsize, and nonnegative
  for (int i = 0; i < index.size(); i++)
  {
    if (index[i] < 0 || index[i] >= htsize)
      stop("index out of bounds");
  }


  // we collect contents in this vector of list
  CharacterVector names = values.names();

  std::vector<List> tmp(htsize);
  std::vector<CharacterVector> tmp_names(htsize);
  for (int i = 0; i < values.size(); i++)
  {
    tmp[index[i]].push_back(values[i]);
    tmp_names[index[i]].push_back(names[i]);
  }

  // compile the output
  List out;
  for (size_t i = 0; i < tmp.size(); i++)
  {
    tmp[i].names() = tmp_names[i];
    out.push_back(tmp[i]);
  }


  return out;
}

/*** R
#zobrist:::MakeHashTable(3, setNames(as.list(1:5), letters[1:5]), c(2,1,0,2,0))
*/
