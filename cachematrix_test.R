##
## cachematrix_test.R
##
## Test module for cachematrix.R
##
## Run all tests with the testCacheMatrix function:
##
##     > testCacheMatrix()
##

source("cachematrix.R")

.myprint <- function (x) message(x)  ## Print plain text to console (e.g. no quotes)
.mymatrixprint <- function (x) print(x)  ## Print matrix with indentation

.testCacheMatrix <- function (cached) {
    verboseOriginalValue <- cached$isVerbose()

    ## Turn on verbose mode
    cached$setVerbose(TRUE)
    
    .myprint("Matrix is:")
    matrix <- cached$get()
    .mymatrixprint(matrix)
    
    .myprint("Request inverse multiple times:") 
    cacheSolve(cached)
    cacheSolve(cached)

    .myprint("Computed (and cached) inverse:")
    computed <- cacheSolve(cached)
    .mymatrixprint(computed)

    .myprint("Expected inverse:")
    expected <- solve(cached$get())
    .mymatrixprint(expected)
    i <- identical(computed, expected)
    .myprint(paste("Are computed and expected inverse identical?", i))
    if (!i) {
        stop("Expected and obtained inverse matrices are not identical!")
    }

    .myprint("Change to identical matrix (inverse shall not be discared):") 
    cached$set(matrix)
    cacheSolve(cached)

    .myprint("Turn verbose off. No message shall be printed till END.")
    cached$setVerbose(FALSE)
    cacheSolve(cached)
    .myprint("END.")    

    .myprint("Restore verbosity on non verbose object and check messages are back:")
    cached$setVerbose(TRUE)
    cacheSolve(cached)

    .myprint("Tests finished")
    
    ## Restore original verbosity
    cached$setVerbose(verboseOriginalValue)
}

testCacheMatrix <- function () {
    original <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
    .myprint("Creating cache-capable object for matrix:")
    cached <- makeCacheMatrix(original)
    cached$setVerbose(TRUE)
    .testCacheMatrix(cached)
    
    alternate <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
    .myprint("Switching to other matrix:")
    cached$set(alternate)
    .testCacheMatrix(cached)
}