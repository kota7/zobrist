#' Create a Zobrist Hash Table
#' @param keysize Positive integer. Bit size of keys
#' @param hashsize Positive integer. Bit size of hash values
#' @param convfunc Function that converts state into key.
#' If not specified, it is assumed that the state is identical to key.
#' If specified, the function must take \code{state} as the first entry,
#' and keyword arguments \code{...}.  See details.
#' @param rehashable Logical. \code{TRUE} if
#' hashsize should be increased dynamically
#' @param threslf Numeric. When \code{rehash = TRUE},
#' rehashing is implemented when the load factor exceeds this value
#' @return zht object
#' @details to be added
#' @export
zht <- function(keysize,
                hashsize = 10, convfunc = NULL,
                rehashable = FALSE, threslf = 0.9)
{
  ## input validation
  stopifnot(hashsize > 0)
  stopifnot(keysize > 0)
  if (hashsize > 32) stop("hashsize cannot be greater than 32")

  ## these are place holder for member fields
  ## they are given values in initialize()
  hashtable <- NULL
  randomint <- NULL
  numkey <- NULL

  ## initialization procedure
  initialize <- function()
  {
    ## initialize hash table ##
    ## hash table is a list of the size 2^hashsize
    ## a table entry is a named list corresponding to a key
    hashtable <<- lapply(1:(2^hashsize), function(a) list())

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


  ## hash operators
  insert <- function(state, value, ...)
  {
    # if value is NULL, call delete
    if (is.null(value)) {
      return(delete(state, ...))
    }

    key <- if (is.null(convfunc)) state else convfunc(state, ...)
    loc <- LocateKey(key, keysize, randomint, hashtable)

    # update the value, then update the name if this is newly added
    hashtable[[ loc[1] ]][[ loc[2] ]] <<- value
    if (loc[3] == 0) {
      # new item, so give name and increment item count
      names(hashtable[[ loc[1] ]])[ loc[2] ] <<- KeyToStr(key, keysize)
      numkey <<- numkey + 1L
    }

    if (rehashable) rehash()
    invisible(self)
  }

  delete <- function(state, ...)
  {
    key <- if (is.null(convfunc)) state else convfunc(state, ...)
    loc <- LocateKey(key, keysize, randomint, hashtable)

    # do nothing if this item does not exists
    if (loc[3] == 0) return(invisible(self))

    hashtable[[ loc[1] ]][[ loc[2] ]] <<- NULL
    numkey <<- numkey-1L

    invisible(self)
  }


  haskey <- function(state, ...)
  {
    key <- if (is.null(convfunc)) state else convfunc(state, ...)
    FindKey(key, keysize, randomint, hashtable)
  }

  getvalue <- function(state, ...)
  {
    key <- if (is.null(convfunc)) state else convfunc(state, ...)
    val <- GetValueByKey(key, keysize, randomint, hashtable)
    # val is 1-length list if key exists, otherwise 0-length list
    if (length(val) == 0) return(NULL)
    unlist(val,recursive = FALSE)
  }


  rehash <- function()
  {
    ## when the load factor = n.key / ht.size becomes too large,
    ## i.e. there are too many keys stored per hash value,
    ## we want to 'rehash' to make a larger hash table

    ## hash size limit is 32
    if (hashsize >= 32) return()

    lf <- numkey / 2^hashsize
    if (lf > threslf) {
      tmp <- hashtable  ## make a copy of old table

      ## set new hash bit size
      hashsize <<- ceiling(log2(numkey/threslf))

      ## then initialize
      initialize()

      ## copy old entries one by one
      tmp <- unlist(tmp, recursive = FALSE)
      keys <- StrsToKeys(names(tmp), keysize)
      hvs  <- ZobristHash_vec(keys, randomint)

      hashtable <<- MakeHashTable(length(hashtable), tmp, hvs)
      ## TODO: performance comparison?
    }

  }

  clone <- function()
  {
    # create a copy of, not a reference to, this object
    out <- zht(keysize, hashsize, convfunc, rehashable, threslf)
    out$hashtable <- hashtable
    out
  }

  size <- function() { numkey }

  self <- environment()
  class(self) <- c("zht")
  return(self)
}




## S3 generic functions

#' Zobrist Hash Table Class
#' @rdname zht-class
#' @description Insert, get, find, and delete methods for Zobrist hash table
#' @details to be added
#' @name zht-class
#' @param x object
#' @param state state object
#' @param value value to be inserted
#' @param ... additional argument to be passed to state->key conversion
#' @return
#' \itemize{
#'   \item{\code{`[<-`} and \code{`[`} returns reference to the object}
#'   \item{\code{haskey} returns logical indicating the existing of the state}
#' }
#' @examples
#' z <- zht(5, 4)
#'
#' z[1:3]
#' z[1:3] <- 15
#' haskey(z, 1:3)
#' z[1:3] <- NULL  # deletion
#' haskey(z, 1:3)


#' @export
#' @rdname zht-class
`[<-.zht` <- function(x, state, ..., value)
{
  x$insert(state, value, ...)
}

#' @export
#' @rdname zht-class
`[.zht` <- function(x, state, ...)
{
  x$getvalue(state, ...)
}

#' @export
#' @rdname zht-class
haskey <- function(x, ...) { UseMethod("haskey") }



#' @export
haskey.zht <- function(x, state, ...)
{
  x$haskey(state, ...)
}


