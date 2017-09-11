## Put comments here that give an overall description of what your
## functions do

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
