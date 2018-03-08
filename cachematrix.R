## This module contains functions for managing
## a matrix-like object that can cache its inverse

## Creates a "matrix" object
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    get <- function() m
    set.inv <- function(i) inv <<- i
    get.inv <- function() inv
    list(set = set, get = get,
         set.inv = set.inv,
         get.inv = get.inv)
}


## Returns inversed matrix, preferably from cache
cacheSolve <- function(x, ...) {
    inv <- x$get.inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$set.inv(inv)
    inv    
}