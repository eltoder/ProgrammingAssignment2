## Functions for caching matrix inverse to avoid repeated computations.
## Usage:
##
##      m <- makeCacheMatrix(matrix_value)
##      s <- cacheSolve(m)
##
## m$set() can be used to change the matrix and clear the cache:
##
##      m$set(matrix_value)
##      s <- cacheSolve(m)

## Returns a list of functions to manipulate a cached matrix and its inverse.
## get()/set() functions access the matrix, and getsolve()/setsolve() access
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedX <- x
    cachedSolve <- NULL
    list(get = function() cachedX,
         set = function(x) { cachedX <<- x; cachedSolve <<- NULL },
         getsolve = function() cachedSolve,
         setsolve = function(solve) cachedSolve <<- solve)
}


## Returns the inverse of a matrix created by makeCacheMatrix(). If already
## calculated, the cached value is returned. Otherwise, calculates the inverse
## via solve() and caches it.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if (is.null(s)) {
        s <- x$setsolve(solve(x$get(), ...))
    }
    s
}
