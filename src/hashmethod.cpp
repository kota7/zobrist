#include <Rcpp.h>
#include <unordered_map>
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
  //   key should be inserted.
  //   if two or more items are to be inserted in the
  //   same hash value entries, then distinct second element
  //   is given

  // record the hypothetical size of each hash value
  std::unordered_map<int, int> add_counter;

  // initialization
  int n = keys.size();
  IntegerMatrix out(n, 3);

  for (int i = 0; i < n; i++)
  {
    IntegerVector key = keys[i];
    int hv = ZobristHash(key, randomint);
    std::string str = KeyToStr(key, keysize);

    // how many items has been "added" so far to this hash value?
    std::unordered_map<int, int>::const_iterator getter = add_counter.find(hv);
    int ct_added = (getter == add_counter.end() ? 0 : getter->second);

    // hypothetical current size
    int hypo_size = hashtable[hv].size() + ct_added;

    if (hashtable[hv].size() == 0) {
      // no entry, this key should be added as the "next" item
      out(i,0) = hv + 1;
      out(i,1) = ct_added + 1;
      out(i,2) = 0;  // not found indicator
      add_counter.insert(std::pair<int, int>(hv, ct_added + 1));
      continue;
    }

    // there is entry, so check the name match
    CharacterVector names = hashtable[hv].names();
    bool found = false;
    for (int j = 0; j < hashtable[hv].size(); j++)
    {
      if (as<std::string>(names[j]) == str) {
        // return one-based index
        out(i,0) = hv + 1;
        out(i,1) = j + 1;
        out(i,2) = 1;   // indicates found
        found = true;
      }
    }

    if (!found) {
      out(i,0) = hv + 1;
      out(i,1) = ct_added + 1; // not found -> append to last
      out(i,2) = 0;  // not found
      add_counter.insert(std::pair<int, int>(hv, ct_added + 1));
    }
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
    if (locations(i,2) == 0) {
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
zobrist:::LocateKeys(list(4, 3), z$keysize, z$randomint, z$hashtable)
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

library(combiter)
z <- zobristht(5, 2, rehashable = FALSE, threslf = 0.5)
## this will accept upto 2^3 / 2 = 4 keys stored
## when we have 4 or more items, rehash is invoked
iter <- isubset(5)
while (hasNext(iter))
{
  i <- nextElem(iter)
  z$update(i, sum(i))
}

## check if the values are still correctly stored
iter <- isubset(5)
while (hasNext(iter))
{
  i <- nextElem(iter)
  cat(z$get(i), sum(i),
      zobrist:::GetValueByKey(i, z$keysize, z$randomint, z$hashtable)[[1]],
      "\n")
}
*/
