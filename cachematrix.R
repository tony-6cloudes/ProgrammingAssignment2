## The functions create a matric which can 
## cache the inverse once it has been calculated since this 
## can be an expensive operation. makeCacheMatrix returns
## an object which has a property to recieve the inverse and store
## it in the cache. cacheSolve takes a matrix, creates an isntance
## of makeCacheMatrix, checks if the inverse is available; if so,
## returns it; otherwise, calculates it and sets the property.

makeCacheMatrix <- function(x = matrix()) {
  ## Returns a special "matrix" object which caches the inverse
  
  mCachedInverse <- NULL
  
  # Getters/Setters
  set <- function(y) {
    x <<- y
    mCachedInverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse)  mCachedInverse <<- inverse
  getInverse <- function() mCachedInverse
  
  list(set = set, get = get, getInverse = getInverse,
       setInverse = setInverse)
}

cacheSolve <- function(x, ...) {
  ## Returns the inverse of the "special" matrix returned by
  ## makeCacheMatrix. If the inverse has already been calculated
  ## and the matrix has not changed, returns the inverse from 
  ## the cache.
  
  # If getInverse is not null, return it
  if(!is.null(x$getInverse())) {
    message("Getting cached data")
    return(x$getInverse())
  }

  # get the matrix, calculate the inverse, set it 
  x$setInverse(solve(x$get(), ...))
  
  # return the inverse
  return(x$setInverse)
}