## Put comments here that give an overall description of what your
## functions do

## makeVector creates setter and getter functions for the matrix to be
## cached

makeVector <- function(x = matrix()) {
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
