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

.testCacheMatrix <- function (cached) {
    print("Matrix is:")
    matrix <- cached$get()
    print(matrix)
    
    print("Request inverse multiple times:") 
    cacheSolve(cached)
    cacheSolve(cached)

    print("Computed (and cached) inverse:")
    computed <- cacheSolve(cached)
    print(computed)

    print("Expected inverse:")
    expected <- solve(cached$get())
    print(expected)
    i <- identical(computed, expected)
    print(paste("Are computed and expected inverse identical?", i))
    if (!i) {
        stop("Not identical expected and obtained inverse matrices!")
    }

    print("Change to identical matrix (inverse shall not be discared:") 
    cached$set(matrix)
    cacheSolve(cached)

    print("Turn verbose off. No message shall be printed till END.")
    cached$setVerbose(FALSE)
    cacheSolve(cached)
    print("END.")    

    print("New non verbose object. No message shall be printed till END.")
    cached <- makeCacheMatrix(matrix)
    cacheSolve(cached)
    cacheSolve(cached)
    print("END.")    
    print("Restore verbosity on non verbose object.")
    cached$setVerbose(TRUE)
    cacheSolve(cached)

    print("")
}

testCacheMatrix <- function () {
    original <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
    print("Creating cache-capable object for matrix:")
    cached <- makeCacheMatrix(original, verbose = TRUE)
    .testCacheMatrix(cached)
    
    alternate <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
    print("Switching to other matrix:")
    cached$set(alternate)
    .testCacheMatrix(cached)
}