## These functions create and operate on a CacheMatrix object that
## wraps a simple invertible matrix.
##
## A CacheMatrix c has the following user functions:
## c$get()         returns the current matrix value
## c$set(m)        sets the cachematrix contents to m (and clears the cache)
##
## and the following functions for internal use only
## c$$getinverse() returns the cached inverse (or NULL)
## c$$setinverse() sets the cached inverse.
##
## The cacheSolve function operates on a CacheMatrix object and either
## returns a cached version of its inverse, or calculates the inverse
## and caches it.

## Creates a CacheMatrix object (optionally taking a vector)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(mm) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of the CacheMatrix x, using a cached value if possible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinverse(inv)
    inv
}
