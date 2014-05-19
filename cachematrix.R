## Inverting a matrix is usually a costly operation. 
## Therefore caching an already inverted matrix can be of some benefit. 
## This module contains functions that allow you to cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse. 
## Note: it is assumed that the matrix x is always invertible
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
	x <<- y # the <<- operator can be used to assign a value 
                # to an object in an environment that is different 
                # from the current environment
	m <<- NULL
    }
    get <- function() x
    setinverted <- function(inverted) m <<- inverted
    getinverted <- function() m
    list(set = set, get = get,
	setinverted = setinverted,
	getinverted = getinverted)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieves the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverted()
    if(!is.null(m)) {
	message("getting cached data")
	return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    m
}
