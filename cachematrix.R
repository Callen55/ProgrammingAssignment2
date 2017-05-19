## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      A <- NULL 
      set <- function(y) {
            x <<- y
            A <<- NULL
      }
      get <- function() x
      setInverse <- function() A <<- solve(x) 
      getInverse <- function() A
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Establishes idienties that can be pulled from ouside the function evniroment and caches the matix and its inverse

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
      ##Checks the cache for the matrix and eitehr returns the sloved matrix, or calcuates it. 
}
