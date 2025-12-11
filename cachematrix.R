## These functions take an invertible matrix, invert it, and store the inverse
## in a cache.  If the inverse for a matrix input is already in the cache, 
## pull the cached inverse.  Otherwise calculate the inverse. 

## makeCacheMatrix creates setter and getter functions for the matrix to be
## cached, and returns a list of functions and variables to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve fetches a cached inverse for a matrix with an inverse 
## previously calculated, and inverts a matrix not cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    View(x)
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
