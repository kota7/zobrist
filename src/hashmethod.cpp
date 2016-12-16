#include <Rcpp.h>
#include "hashfunc.h"
#include "keyStrConversion.h"

using namespace Rcpp;


IntegerVector LocateKey(IntegerVector &key, int keysize,
                        std::vector<unsigned int> &randomint,
                        ListOf<List> &hashtable)
{
  IntegerVector out(3);
  int hv = ZobristHash(key, randomint);
  std::string str = KeyToStr(key, keysize);

  if (hashtable[hv].size() == 0) {
    // no entry, this key should be the first entry
    out[0] = hv + 1;
    out[1] = 1;
    out[2] = 0;  // not found
    return out;
  }

  CharacterVector names = hashtable[hv].names();
  for (int i = 0; i < hashtable[hv].size(); i++)
  {
    if (as<std::string>(names[i]) == str) {
      // return one-based index
      out[0] = hv + 1;
      out[1] = i + 1;
      out[2] = 1;   // indicates found
      return out;
    }
  }
  // not found
  out[0] = hv + 1;
  out[1] = hashtable[hv].size() + 1; // not found -> append to last
  out[2] = 0;  // not found

  return out;
}


IntegerMatrix LocateKeys(ListOf<IntegerVector> &keys,
                         int keysize,
                         std::vector<unsigned int> &randomint,
                         ListOf<List> &hashtable)
{
  // locate keys in hashtable
  //
  // returns integer matrix, where
  //   each row corresponds each key
  //   and three columns exists,
  //     col 0: index of list
  //     col 1: index within list
  //     col 2: 0/1, 1 indicates the key exists
  //   when the key is not in the hashtable,
  //   it returns indices of the location where this
  //   key should be inserted

  // obtain hashvalues
  //
  int n = keys.size();
  IntegerMatrix out(n, 3);
  for (int i = 0; i < n; i++)
  {
    IntegerVector key = keys[i];
    IntegerVector row = LocateKey(key, keysize, randomint, hashtable);
    for (int j = 0; j < 3; j++) out(i,j) = row(j);
  }

  return out;
}



List GetValueByKey(IntegerVector &key, int keysize,
                   std::vector<unsigned int> &randomint,
                   ListOf<List> &hashtable)
{
  IntegerVector location = LocateKey(key, keysize, randomint, hashtable);
  if (location[2] == 0) return NULL;
  return hashtable[location[0]-1][location[1]-1];
}


List GetValueByKeys(ListOf<IntegerVector> &keys,
                    int keysize,
                    std::vector<unsigned int> &randomint,
                    ListOf<List> &hashtable)
{
  IntegerMatrix locations = LocateKeys(keys, keysize, randomint, hashtable);
  List out;
  for (int i = 0; i < keys.size(); i++)
  {
    if (locations(i,20) == 0) {
      List tmp;
      out.push_back(tmp);
    } else {
      out.push_back(hashtable[locations(i,0)-1][locations(i,1)-1]);
    }
  }
  return out;
}



bool FindKey(IntegerVector &key, int keysize,
             std::vector<unsigned int> &randomint,
             ListOf<List> &hashtable)
{
  return (LocateKey(key, keysize, randomint, hashtable)[2] == 1);
}

LogicalVector FindKeys(ListOf<IntegerVector> &keys, int keysize,
                       std::vector<unsigned int> &randomint,
                       ListOf<List> &hashtable)
{
  int n = keys.size();
  LogicalVector out(n);
  IntegerMatrix locations = LocateKeys(keys, keysize, randomint, hashtable);
  for (int i = 0; i < n; i++) out[i] = (locations(i,2) == 1);

  return out;
}



/*** R
library(zobrist)
z <- zobristht(5, 4)
zobrist:::LocateKey(4, z$keysize, z$randomint, z$hashtable)
zobrist:::LocateKeys(list(3, 4), z$keysize, z$randomint, z$hashtable)
zobrist:::FindKey(3, z$keysize, z$randomint, z$hashtable)
zobrist:::FindKeys(list(3, 4), z$keysize, z$randomint, z$hashtable)

z$update(3, 2)

zobrist:::LocateKeys(list(3, 4), z$keysize, z$randomint, z$hashtable)
zobrist:::LocateKey(3, z$keysize, z$randomint, z$hashtable)
zobrist:::FindKey(3, z$keysize, z$randomint, z$hashtable)
zobrist:::FindKeys(list(3, 4), z$keysize, z$randomint, z$hashtable)

zobrist:::GetValueByKey(4, z$keysize, z$randomint, z$hashtable)
zobrist:::GetValueByKey(3, z$keysize, z$randomint, z$hashtable)

zobrist:::GetValueByKeys(list(4, 3), z$keysize, z$randomint, z$hashtable)

z$update(c(4, 1), 10)
zobrist:::LocateKey(c(4, 1), z$keysize, z$randomint, z$hashtable)
zobrist:::LocateKeys(list(3, 4, c(4, 1)), z$keysize, z$randomint, z$hashtable)
zobrist:::GetValueByKey(c(4, 1), z$keysize, z$randomint, z$hashtable)
zobrist:::GetValueByKeys(list(3, 4, c(4, 1, 4, 1, 1, 4)),
                         z$keysize, z$randomint, z$hashtable)

zobrist:::FindKey(c(1, 4), z$keysize, z$randomint, z$hashtable)
zobrist:::FindKeys(list(3, 4), z$keysize, z$randomint, z$hashtable)
zobrist:::FindKeys(list(3, 4, c(4, 4, 1, 4), c(3, 3, 3), c(3, 3, 4)),
                   z$keysize, z$randomint, z$hashtable)


*/
