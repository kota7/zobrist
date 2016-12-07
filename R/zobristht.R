#' Create a Zobrist Hash Table
#' @param keysize Positive integer. Bit size of keys
#' @param hashsize Positive integer. Bit size of hash values
#' @param rehashable Logical. \code{TRUE} if
#' hashsize should be increased dynamically
#' @param threslf Numeric. When \code{rehash = TRUE},
#' rehashing is implemented when the load factor exceeds this value
#' @param memorysize Nonnegative integer. This specifies how many recent
#' hash values to be memorized for a quick access
#' @return zobristhash object
#' @export
zobristht <- function(keysize, hashsize,
                      rehashable = FALSE, threslf = 0.9,
                      memorysize = 10)
{
  ## input validation
  ### TODO: maximum hashsize? keysize?
  ###       limit for memory size?

  ## these are place holder for member fields
  ## they are given values in initialize()
  hashtable <- NULL
  quickmap <- NULL
  randomint <- NULL
  numkey <- NULL


  ## initialization procedure
  initialize <- function()
  {
    ## initialize hash table ##
    ## hash table is a list of the size 2^hashsize
    ## a table entry is a named list corresponding to a key
    hashtable <<- lapply(1:(2^hashsize), function(a) list())

    ## initializes quick memory map ##
    ## this is a named integer vector of short size, where
    ## the names represents the key and values represents the hash value
    ## used to obtain hash values computed recently
    ## At first, the vector is named with letters so it won't match any
    ## keys.
    quickmap <<- setNames(integer(memorysize), LETTERS[1:memorysize])

    ## generate random integers for each key positions
    ## i-th number represents the hash value for a key such that
    ## all but the i-th position is 1
    if (keysize <= 2^hashsize - 1) {
      randomint <<- sample.int(2^hashsize - 1, keysize)
    } else {
      randomint <<- sample.int(2^hashsize - 1, keysize, replace = TRUE)
    }
    ## number of keys stored
    numkey <<- 0L
  }

  initialize()



  ## define hash function
  hashfunc <- function(key, incr = integer(0))
  {
    # key     : an integer vector representing the positive key entries
    # incr : an integer vector representing additional entries

    str1 <- KeyToStr(key, keysize)
    str2 <- KeyToStr(c(key, incr), keysize)


    ## first, check if bitstr2 is in the memory.
    ## if it is, we are done. just return the hash value
    flg <- as.character(str2) == names(quickmap)
    if (any(flg)) {
      index <- head(which(flg), 1)
      value <- quickmap[index]
      # re-order the quickmap so that the current hash value is the last
      quickmap <<- c(quickmap[-index], quickmap[index])
      return(unname(value))
    }

    ## second, check if bitstr1 is in the memory.
    ## if it is, then we can compute the hash value of bitstr2
    ## easily by XOR-ing the offsets
    flg <- as.character(str1) == names(quickmap)
    if (any(flg)) {
      index <- head(which(flg), 1)
      value <- quickmap[index]
      quickmap <<- c(quickmap[-index], quickmap[index])
      ## value for str2
      value <- Reduce(bitwXor, randomint[incr], value)
      # add the new value to the quick map
      quickmap <<- c(quickmap[-1], setNames(value, str2))
      return(unname(value))
    }

    ## third, we will compute the hash value from scratch
    value <- Reduce(bitwXor, randomint[c(key, incr)], 0L)
    quickmap <<- c(quickmap[-1], setNames(value, str2))
    return(unname(value))
  }

  hashfunc_vec <- function(keys)
  {
    ## vectorized hash function
    ##
    ## keys: list of integer vectors

    ### deploy by Rcpp
    hashfunc_vec_cpp(keys, randomint)
  }



  ## define methods
  ## - update(key, value)
  ## - delete(key)
  ## - find(key)
  ## - get(key)
  locate <- function(key, incr = integer(0))
  {
    # this function search for c(key, incr) in the hash table
    # if if exists, returns a size 3 integer vector (i, j, k), such that
    # hashtable[[i]][[j]] stores the key
    # k equals 1 if and only if this key already exists
    hv <- hashfunc(key, incr)
    i <- hv + 1  # one-based index
    str <- KeyToStr(c(key, incr), keysize)

    ## do we have this key already?
    flg <- str == names(hashtable[[i]])
    if (any(flg)) {
      ## there is one already
      j <- head(which(flg), 1)
      k <- 1L
    } else {
      j <- length(NULL) + 1L
      k <- 0L
    }
    c(i, j, k)
  }

  update <- function(key, value, incr = integer(0))
  {
    index <- locate(key, incr)
    str <- KeyToStr(c(key, incr), keysize)
    if (index[[3]] == 1L) {
      hashtable[[index[1]]][[index[2]]] <<- value
    } else {
      hashtable[[index[1]]] <<- c(hashtable[[index[1]]],
                                  setNames(list(value), str))
      ## increment numkey
      numkey <<- numkey + 1L
      if (rehashable) rehash()  ## rehash if needed
    }
    invisible(self)
  }

  delete <- function(key, incr = integer(0))
  {
    index <- locate(key, incr)
    if (index[[3]] == 1L) {
      hashtable[[index[1]]][[index[2]]] <<- NULL
      ## decrement numkey
      numkey <<- numkey - 1L
    }
    invisible(self)
  }

  find <- function(key, incr = integer(0))
  {
    locate(key, incr)[3] == 1L
  }

  get <- function(key, incr = integer(0))
  {
    index <- locate(key, incr)
    if (index[3] == 1L) {
      return(hashtable[[index[1]]][[index[2]]])
    } else {
      return(NULL)
    }
  }


  rehash <- function()
  {
    ## when the load factor = n.key / ht.size becomes too large,
    ## i.e. there are too many keys stored per hash value,
    ## we want to 'rehash' to make a larger hash table

    lf <- numkey / 2^hashsize
    if (lf > threslf) {
      tmp <- hashtable  ## make a copy of old table

      ## set new hash bit size
      hashsize <<- ceiling(log2(numkey/threslf))

      ## then initialize
      initialize()

      ## copy old entries one by one
      tmp <- unlist(tmp, recursive = FALSE)
      #cat("length before = ", length(tmp), "\n")
      keys <- StrsToKeys(names(tmp), keysize)
      hvs  <- hashfunc_vec_cpp(keys, randomint)

      #Map(update, keys, tmp)
      hashtable <<- MakeHashTable(length(hashtable), tmp, hvs)
      ## TODO: performance comparison?

    }

  }

  self <- environment()
  class(self) <- "zoristht"
  return(self)
}


