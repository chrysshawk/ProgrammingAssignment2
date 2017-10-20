## Fuction pair using lexical scoping to store the inverse of a matrix to cache and another
## function to call upon that function if the result is not already in cache and return the answer

## Function to create a matrix for calculating the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
     ## resetting variables & functions
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     ## defining get- and set functions for calculating the inverse matrix
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     
     ## adding functions to a named list in matrix
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## Function to return the inverse of x, checking first to see if answer is cached
cacheSolve <- function(x, ...) {
     ## return a matrix that is the inverse of 'x'
     m <- x$getinv()
     
     ## returning the inversed matrix if it is already cached in parent environment
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## if inversed matrix is not cached, calculate it using solve function
     data <- x$get()
     m <- solve(data, ...)
     
     ## set the new inversed matrix calculation
     x$setinv(m)
     
     ## return result
     m
}
