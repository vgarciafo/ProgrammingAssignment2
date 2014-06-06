## This functions can be used to create an special matrix that can cache
## its inverse. The function makeCacheMatrix is the function that actualy 
## creates de special matrix and the function cacheSolve computes de inverse
## and returns it in the case that the inverse was not already cached. If the 
## inverse was cached cacheSolve returns the cached value.

## makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
      	x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv  <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix. If the inverse was not cache, this function computes it.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
      if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
