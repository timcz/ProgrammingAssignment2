## This file contains functions that create a special "Matrix" object and compute and cache
## the inverse of that matrix object.
##
## Timothy Czyrnyj -- 2014-12-21

## makeCacheMatrix -- creates and returns the special matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL ## if we change the matrix, the cached inverse is no longer valid
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve -- accepts the "Matrix" object created by makeCacheMatrix as an argument
##               and returns its inverse
cacheSolve <- function(x, ...) {
        
  inv <- x$getInverse()
  if(!is.null(inv)) {  ## if we have cached data...
    message("getting cached data")
    return(inv)
  } 
  
  ## compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
