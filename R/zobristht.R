#' Create a Zobrist Hash Table
#' @param keysize Positive integer. Bit size of keys
#' @param hashsize Positive integer. Bit size of hash values
#' @param rehash Logical. \code{TRUE} if
#' hashsize should be increased dynamically
#' @param threslf Numeric. When \code{rehash = TRUE},
#' rehashing is implemented when the load factor exceeds this value
#' @param memorysize Nonnegative integer. This specifies how many recent
#' hash values to be memorized for a quick access
#' @return zobristhash object
#' @export
zobristht <- function(keysize, hashsize,
                      rehash = FALSE, threslf = 0.9,
                      memorysize = 10)
{
  ## input validation
  ### TODO: maximum hashsize? keysize?
  ###       limit for memory size?

  ## initialize hash table ##
  ## hash table is a list of the size 2^hashsize
  hashtable <- rep(list(), 2^hashsize)

  ## initializes quick memory map ##
  ## this is a named integer vector of short size, where
  ## the names represents the key and values represents the hash value
  ## used to obtain hash values computed recently
  ## At first, the vector is named with letters so it won't match any
  ## keys.
  quickmap <- setNames(integer(memorysize), LETTERS[1:memorysize])

  ## generate random integers for each key positions
  ## i-th number represents the hash value for a key such that
  ## all but the i-th position is 1
  randomint <- sample.int(2^hashsize, keysize)


  ## define hash function
  hashfunc <- function(key, offsets = integer(0))
  {
    # key     : an integer vector representing the positive key entries
    # offsets : an integer vector representing additional entries

    bitstr1 <- intvec_to_bitstring(key, keysize)
    bitstr2 <- intvec_to_bitstring(c(key, offsets), keysize)
    #cat("bitstr1 = ", bitstr1, "\n")
    #cat("bitstr2 = ", bitstr2, "\n")


    ## first, check if bitstr2 is in the memory.
    ## if it is, we are done. just return the hash value
    flg <- as.character(bitstr2) == names(quickmap)
    if (any(flg)) {
      #cat(bitstr2, "found in", which(flg), "\n")
      index <- head(which(flg), 1)
      value <- quickmap[index]
      # re-order the quickmap so that the current hash value is the last
      quickmap <<- c(quickmap[-index], quickmap[index])
      return(unname(value))
    }

    ## second, check if bitstr1 is in the memory.
    ## if it is, then we can compute the hash value of bitstr2
    ## easily by XOR-ing the offsets
    flg <- as.character(bitstr1) == names(quickmap)
    if (any(flg)) {
      #cat(bitstr1, "found in", which(flg), "\n")
      index <- head(which(flg), 1)
      value <- quickmap[index]
      quickmap <<- c(quickmap[-index], quickmap[index])
      ## value for bitstr2
      value <- Reduce(bitwXor, randomint[offsets], value)
      # add the new value to the quick map
      quickmap <<- c(quickmap[-1], setNames(value, bitstr2))
      return(unname(value))
    }

    ## third, we will compute the hash value from scratch
    value <- Reduce(bitwXor, randomint[c(key, offsets)], 0L)
    quickmap <<- c(quickmap[-1], setNames(value, bitstr2))
    return(unname(value))
  }

  ## define methods
  ## - update(key, value)
  ## - delete(key)
  ## - find(key)
  ## - get(key)
  insert <- function(key, value)
  {

  }

  self <- environment()
  class(self) <- "zoristht"
  return(self)
}




intvec_to_bitstring <- function(key, keysize)
{
  # internal function to convert a key given in integer vector
  # into a bitstring (reverser order)
  #
  # key:     integer vector that represents the position of positive keys
  # keysize: bit size of key
  #
  # returns:
  #   bistring in a reverse order

  #cat("key = ", key, "\n")
  if (!is.numeric(key)) stop("key must be an integer vector")

  key <- as.integer(key)  # just in case key includes decimals
  # we can only use the indices between 1 to keysize
  # warn if key includes any number out of bounds
  check <- (key >= 1L) & (key <= keysize)
  if (!all(check)) {
    warning("Following indices are out-of-bounds and ignored:\n ",
            paste0(key[!check], collapse = ", "), "\n")
    key <- key[check]
  }

  out <- rep("0", keysize)
  out[key] <- "1"
  paste0(out, collapse = "")
}


