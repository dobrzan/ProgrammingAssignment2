## -----------------
## Functions below are part of an example on how to 
## cache potentially time consuming computations
## -----------------

## Creates a special matrix object that can cache its inverse.

## 'x' is a matrix for which its inverse is cached
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, 
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)    
}

## Returns computed the inverse of the special matrix created with 
## 'makeCacheMatrix' function. The inverse is returned from cache (if possible),
## or calculated (and cached) when executed.

## 'x' is a special matrix object that can cache its inverse
cacheSolve <- function(x, ...) {
    stopifnot(!is.na(x))
    s <- x$getSolve()
    if (!is.null(s)) {
        message("gettting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}