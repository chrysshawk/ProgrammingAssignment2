## Fuction pair using lexical scoping to store the inverse of a matrix to cache and another
## function to call upon that function if the result is not already in cache and return the answer

## Function to create a matrix for calculating the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     print(x)
     print(m)
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Function to return the inverse of x, checking first to see if answer is cached
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
}
