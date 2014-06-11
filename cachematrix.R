## Functions to make the inverse of a matrix cacheable 
# so that the inverse doesn't need to be repeatedly calculated.
#
## General Usage:
# create a cacheable matrix from scratch (NULL) or any other matrix
# using makeCacheMatrix.
#
# contents of the cacheable matrix can be updated using $set() 
#
## NB:  to retrieve the actual matrix from the cache wrapper,
# use the $get() function.
#
## Cache Management
# 'dirty' is automatically detected when the $set is used
# NB:  equality is NOT tested -- any new set invalidates
# the cache
#
# The messge 'getting cached data' is output to the console 
# when the cache is accessd.

#
## makeCacheMatrix( matrix ) returns list
#
# pass any matrix or nil.  
# a list containing the matrix as an element
# along with the getter/setter is returned.
# use this returned list to access cached inverses
#
# as no validation is done on the inv relative to x,
# function could be adapted to cache ANY feature or object
# relative to some base object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           # cache of x's inverse -- initially NULL (aka invalid)
  
  # setter function used to set/update the matrix contained in the cache wrapper
  # access with $set() call with any matrix as argument
  set <- function(y) {
    x <<- y             # copy new matrix to list
    inv <<- NULL        # invalidate the cache
  }
  
  # getter function to return the matrix
  # acccess with $get() and use like any other matrix reference
  get <- function() x
  
  # sets the inverse of x to be cached
  # normally called by cacheSolve -- see below
  setinv <- function(solve) inv <<- solve # set inv to the new value -- note no validation
  
  # access the cached item
  getinv <- function() inv
  
  # sets up the list of functions to manage the cache
  # note that the actual matrix object is NOT embedded IN 
  # list (a la Object programming), rather it is referenced
  # as a lexical context from whence this function is invoked
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve (cachedmatrix)
# returns the inverse of the matrix used to set up the cache as above
#
# NB:  the message 'getting cached data' is printed when the cache is 
# referenced.  
#
# Usage:  call cacheSolve on the list returned from makeCacheMatrix
# to get the inverse of the matrix
cacheSolve <- function(x, ...) {    # x is the list object created in makeCacheMatrix
  
  inv <- x$getinv()                 # retrieve the object currently cached
  if(!is.null(inv)) {
    # cached object is valid -- use it!
    message("getting cached data")
    return(inv)
  }
  ## fall through - R:  cached object is INVALID -- need to update it
  
  data <- x$get()                   # fetch the base object/matrix
  inv <- solve(data, ...)           # use base solve function to compute inverse
                                    # NB: could be generalized to use ANY fuction...
  x$setinv(inv)                     # store object in the cache
  inv  
}
