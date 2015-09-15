##
## cachematrix.R
##
## Cache support for matrix inverse
##
## The makeCacheMatrix() creates a cache capable object out of a regular
## matrix:
##
##     > original <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
##     > cacheCapable <- makeCacheMatrix(original)
##
## The cacheSolve(cacheCapable, ...) computes the inverse of the matrix by
## means of the solve(matrix, ...) function.
## 
## The inverse is cached so subsequent calls to cacheSolve(cacheCapable, ...)
## shall return this cached inverse instead of calling the costly solve
## function again.
##
##     > cacheSolve(cached)
##     ## solve will be executed
##     > cacheSolve(cached)
##     ## cached inverse is returned
##     > cacheSolve(cached)
##     ## cached inverse is returned
##     > cacheSolve(cached)
##
## The matrix of the cache capable object can be changed by the set member.
##
##     > cacheCapable$set(newMatrix)
##
## The cached inverse will be discarded if the newMatrix is different.
##
## Verbose sample
##
##     > original <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
##     > cached <- makeCacheMatrix(original, verbose = TRUE)
##     > cacheSolve(cached)
##     Inverse computed.
##          [,1] [,2] [,3]
##     [1,]  -24   18    5
##     [2,]   20  -15   -4
##     [3,]   -5    4    1
##     > cacheSolve(cached)
##     Inverse retrieved from cache.
##          [,1] [,2] [,3]
##     [1,]  -24   18    5
##     [2,]   20  -15   -4
##     [3,]   -5    4    1
##     > cacheSolve(cached)
##     Inverse retrieved from cache.
##          [,1] [,2] [,3]
##     [1,]  -24   18    5
##     [2,]   20  -15   -4
##     [3,]   -5    4    1
##     > alternate <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
##     > cached$set(alternate)
##     Matrix changed. Discarding any previous cached inverse.
##     > cacheSolve(cached)
##     Inverse computed.
##          [,1] [,2]
##     [1,]   -2    3
##     [2,]    3   -4
##     > cacheSolve(cached)
##     Inverse retrieved from cache.
##          [,1] [,2]
##     [1,]   -2    3
##     [2,]    3   -4
##     > cached$set(alternate)
##     New matrix is identical. Nothing will be changed.
##     > cacheSolve(cached)
##     Inverse retrieved from cache.
##          [,1] [,2]
##     [1,]   -2    3
##     [2,]    3   -4
##     > 


##
## Creates a matrix object capable of caching its inverse. The returned object
## can be used as parameter for the cacheSolve function. This matrix 
##
## Arguments:
##
##     matrix = matrix()  The original matrix object
##     verbose = FALSE    Verbose operation via console messages  
##
## Returns a list consisting of the following methods:
##
##     set(newMatrix)  Setter to change the matrix object
##     get()           Accessor to the original matrix object
##     .setInverse(c)  Sets the cached inverse (see cacheSolve function)
##     .getInverse()   Gets the cached inverse (see cacheSolve function)
##                     
##
makeCacheMatrix <- function(matrix = matrix(), verbose = FALSE) {
    isVerbose <- function () verbose
    setVerbose <- function (newVerbose) verbose <<- newVerbose
    inverse <- NULL
    set <- function (newMatrix) {
        if (identical(matrix, newMatrix)) {
            if (isVerbose()) {
                message("New matrix is identical. Nothing will be changed.")
            }
        } else {
            if (isVerbose()) {
                message("Matrix changed. Discarding any previous cached inverse.")
            }
            matrix <<- newMatrix
            inverse <<- NULL
        }
    }
    get <- function () matrix
    .setInverse <- function (newInverse) inverse <<- newInverse
    .getInverse <- function () inverse
    r <- list(
        setVerbose = setVerbose,  ## sets verbose operation on or off
        isVerbose = isVerbose,    ## predicate of verbose operation
        set = set,  ## setter to change the underlying matrix
        get = get,  ## getter for the underlying matrix
        .setInverse = .setInverse,  ## setter for the inverse matrix cache
        .getInverse = .getInverse   ## getter for the inverse matrix cache
    )
    
    invisible(r)
}

##
## solve() variant for the cache matrix
##
## Arguments:
##
##    cached  Cache capable matrix created with the makeCacheMatrix function
##    ...     Extra parameters to the solve() function
##
## Returns:
##
##    The inverse matrix from the cache if available, or just computed by
##    the solve() function.
## 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Check inverse availability
    if (is.null(x$.getInverse())) {
        # Inverse not found. Compute and set to the cached matrix
        x$.setInverse(solve(x$get(), ...))
        if (x$isVerbose()) {
            message("Inverse computed.")
        }
    } else {
        if (x$isVerbose()) {
            message("Inverse retrieved from cache.")
        }
    }
    
    # Inverse already computed here.
    x$.getInverse()
}
