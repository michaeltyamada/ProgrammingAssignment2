## Functions to invert a matrix using caching
# makeCacheMatrix - wraps a matrix into a makeCacheMatrix object
# cacheSolve - gets the inverted matrix from cache or calcs it

## Create object makeCacheMatrix for use with cacheSolve
makeCacheMatrix <- function(x = matrix()) {

     # x represents matrix to be inverted
     # i represents inverse of the matrix

     i <- NULL
     set <- function(my_matrix) {
          x <<- my_matrix
     }
     get <- function() x
     getInverse <- function() i
     setInverse <- function(my_matrix) i <<- my_matrix
     list(set = set, get = get,
          getInverse = getInverse,
          setInverse = setInverse)
     
     
}


## Invert a matrix using cachingsolve(data)
# Requires an makeCacheMatrix object as input
# Assumes that the matrix is invertable (no error handling)
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     # Get cached data, if exists
     i <- x$getInverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     
     # Otherwise calc inverse and update cache
     data <- x$get()
     i <- solve(data)
     x$setInverse(i)
     i
}
