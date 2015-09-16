##
## cachematrix.R
##
## Cache support for matrix inversion.
##
## Two functions are provided:
##
##     makeCacheMatrix
##     cacheSolve
##
## The makeCacheMatrix() creates an "matrix" object capable of keeping a
## cache of the inverse matrix instead of re-computing it again.
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
## The internal matrix object (the actual values) of the cache object can be
## changed by means of the set member function:
##
##     > cacheCapable$set(newMatrix)
##
## At this call, the cached inverse will be discarded if the newMatrix is
## different. Inverse will be computed with the solve() method at the next
## call to cacheSolve(cached).
##
## The object can be set in verbose mode so messages are shown in the console
## informing about the internal processing. Enable (or disable) verbosity
## by means the setVerbose member function:
## 
##    > caches$setVerbose(TRUE)
##
## Sample:
##
##     > original <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
##     > cached <- makeCacheMatrix(original)
##     > cached$setVerbose(TRUE)
##     ## Cache matrix is now in verbose mode. Will inform of inverse
##     ## calculation, cache hit or internal matrix modification.
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
##     ## Change internal matrix to
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
##     ## Disable verbose mode. No messages about operation will be printed.
##     > cached$setVerbose(FALSE)
##     > cacheSolve(cached)
##          [,1] [,2]
##     [1,]   -2    3
##     [2,]    3   -4
##     > 

## Internal function for verbose operation of the matrix
.cacheMatrixMessage <- function (verbose, m) {
    if (verbose) {
        message(paste("*** [cachematrix]:", m))
    }
}

##
## Creates a matrix object capable of caching its inverse. The returned object
## can be used as parameter for the cacheSolve function. This matrix 
##
## Arguments:
##
##     matrix = matrix()  The original matrix object
##
## Returns a list consisting of the following methods:
##
##     set(newMatrix)  Setter to change the matrix object
##     get()           Accessor to the original matrix object
##
##     setVerbose()    Setter to change the verbosity of the object
##     isVerbose()     Predicate to tell whether the object is verbose or not
##
##     .setInverse(c)  Sets the cached inverse (see cacheSolve function)
##     .getInverse()   Gets the cached inverse (see cacheSolve function)
##                     
##
makeCacheMatrix <- function(x = matrix()) {
    verbose <- FALSE
    isVerbose <- function () verbose
    setVerbose <- function (newVerbose) verbose <<- newVerbose
    inverse <- NULL
    set <- function (newMatrix) {
        if (identical(x, newMatrix)) {
            .cacheMatrixMessage(isVerbose(), "New matrix is identical. Nothing will be changed.")
        } else {
            .cacheMatrixMessage(isVerbose(), "Matrix changed. Discarding any previous cached inverse.")
            x <<- newMatrix
            inverse <<- NULL
        }
    }
    get <- function () x
    .setInverse <- function (newInverse) inverse <<- newInverse
    .getInverse <- function () inverse
    
    ## Compose the matrix object
    list(
        set = set,  ## setter to change the underlying matrix
        get = get,  ## getter for the underlying matrix
        setVerbose = setVerbose,  ## sets verbose operation on or off
        isVerbose = isVerbose,    ## predicate of verbose operation
        .setInverse = .setInverse,  ## setter for the inverse matrix cache
        .getInverse = .getInverse   ## getter for the inverse matrix cache
    )
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
        .cacheMatrixMessage(x$isVerbose(), "Inverse computed.")
    } else {
        .cacheMatrixMessage(x$isVerbose(), "Inverse retrieved from cache.")
    }
    
    # Inverse already computed here.
    x$.getInverse()
}
