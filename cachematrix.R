## This module contains functions for managing
## a matrix-like object that can cache its inverse

## Creates a "matrix" object with a cache-able inverse
makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(new.matrix) {
        m <<- new.matrix
        inverse <<- NULL
    }
    get <- function() m
    set.inverse <- function(new.inverse) inverse <<- new.inverse
    get.inverse <- function() inverse
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Returns inversed matrix, preferably from cache
cacheSolve <- function(x, ...) {
    inverse <- x$get.inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    new.inverse <- solve(x$get(), ...)
    x$set.inverse(new.inverse)
    new.inverse    
}