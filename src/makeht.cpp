#include <Rcpp.h>
#include <string>
#include "hashfunc.h"
#include "keyStrConversion.h"
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


// [[Rcpp::export]]
List RemakeHashTable(List &oldtable, int htsize,
                     int keysize, std::vector<unsigned int> &randomint)
{
  std::vector<List> tmp(htsize);
  std::vector<CharacterVector> tmp_names(htsize);

  for (int i = 0; i < oldtable.size(); i++)
  {
    List cur_list = oldtable[i];
    if (cur_list.size() == 0) continue;

    CharacterVector names = cur_list.names();
    for (int j = 0; j < cur_list.size(); j++)
    {
      IntegerVector key = StrToKey(as<std::string>(names[j]), keysize);
      int hv = ZobristHash(key, randomint);
      tmp[hv].push_back(cur_list[j]);
      tmp_names[hv].push_back(names[j]);
    }
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
*/
