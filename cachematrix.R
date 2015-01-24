## These functions allow to work with matrices and their inverses through a special wrapper.
## The inverse matrices can be cached so we can save computation time.

## Create a wrapper around a matrix and its inverse to make this cacheable
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(newX) {
        x <<- newX
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(newInv) inv <<- newInv
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Obtain the inverse of a wrapped matrix, trying to fetch it from the cache
## (if available) or calculating it instead (caching the result in the wrapper)
cacheSolve <- function(x, ...) {
    # Try to fetch the inverse from the cache
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Calculate the inverse and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    inv
}