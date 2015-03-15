## makeCacheMatrix - Creates a cached matrix and provides a list of
##                   functions for caching the matrix and its inverse.
## cacheSolve - Determines whether a cached inversion of x exists and
##              returns it. Otherwise, solves x, caches it, and returns
##              it.

## Initializes the variable m_inverseX and creates a list of 4
## functions. Provides caching functionality for the 'cacheSolve'
## function.
##      1. SetMatrix(y) - stores y to the cached matrix x and
##          clears the cached m_inverseX
##      2. GetMatrix() - returns the cached matrix x (set by 
##          'SetMatrix(y)' or the initial call of 'makeCacheMatrix(x)')
##      3. SetInverse(inverse) - caches the matrix passed via
##          'inverse' to m_inverseX.
##      4. GetInverse() - returns the contents of m_inverseX
##          (set by SetInverse(inverse)).

makeCacheMatrix <- function(x = matrix()) {
    m_inverseX <- NULL
    f_set <- function(y) {
        x <<- y
        m_inverseX <<- NULL
    }
    f_get <- function() x
    f_setInverse <- function(inverse) m_inverseX <<- inverse
    f_getInverse <- function() m_inverseX
    # Return
    list(SetMatrix = f_set,
         GetMatrix = f_get,
         SetInverse = f_setInverse,
         GetInverse = f_getInverse)
}


## Uses the list of functions created by 'makeCacheMatrix'
## to cache the inverse of the matrix 'x'. If 'GetInverse()'
## returns null, then solve the cached matrix and cache the
## result via 'SetInverse(inverse)'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invertX <- x$GetInverse()
    if(!is.null(invertX)) {
        message("getting cached data")
        return(invertX)
    }
    data <- x$GetMatrix()
    invertX <- solve(data, ...)
    x$SetInverse(invertX)
    invertX
}