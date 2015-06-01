## This file contains a pair of functions that can be used together to 
## calculate and cache a matrix's inverse. This allows the system to  
## do the expensive inverse calculation only once per matrix.

## makeCacheMatrix creates a list that contains four functions that can be used in caching a matrix inverse:
## set - sets the value of the matrix
## get - gets the value of the matrix
## setInverse - sets the value of the inverse
## getInverse - gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
   myInverse <- NULL
   set <- function(y) {
      x <<- y
      myInverse <<- NULL
   }
   get <- function() x
   setInverse <- function(newInverse) myInverse <<- newInverse
   getInverse <- function() myInverse
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve calculates the Inverse of the special "matrix" created with makeCacheMatrix. 
## It first checks to see if the Inverse has already been calculated and cached. 
## If so, it uses the Inverse from the cache. 
## Otherwise, it calculates the Inverse of the data and 
## sets the value of the inverse in the cache.
## Assumes matrix is an invertible square matrix.
cacheSolve <- function(x, ...) {
   inverse <- x$getInverse()
   if(is.null(inverse)) {
      inverse <- solve(x$get(), ...)
      x$setInverse(inverse)
   }
   return(inverse)   
}
