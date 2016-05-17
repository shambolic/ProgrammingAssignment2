## Coursera R Programming - Assignment 2
## submitted by:  Jay Johnston - 17-5-16

makeCacheMatrix <- function(x = matrix()) {
## creates a special matrix object that can cache its inverse
        inv <- NULL                           # initialise inverse to Null
        set <- function(y) {
                x <<- y  
                inv <<- NULL
        }
        get <- function() x                    # return matrix
        setInverse <- function(sl) inv <<- sl  # set parent inverse to solved
        getInverse <- function() inv           
        list(set = set, get = get,             # return a list of functions
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
## Calculate or return a matrix that is the inverse of 'x'
        inv <- x$getInverse() 
        if(!is.null(inv)) {       # checks to see if it exists in memory
                message("getting cached data...")
                return(inv)       # returns inv if it already exists (i.e. is cached)
        }
        data <- x$get()           #otherwise it sets data to value of matrix 
        inv <- solve(data, ...)   # inverts/ solves matrix
        x$setInverse(inv)         # calls setInverse to set value in parent environment
        inv                       # prints inverse
}

message("Tests:\ncreating 4x4 test.matrix....")
test.matrix = makeCacheMatrix(matrix(c(1:4), nrow=2, ncol=2))
message("calling test.matrix$get().... ")
test.matrix$get()
message("calling cacheSolve on test.matrix.... ")
cacheSolve(test.matrix)
message("calling cacheSolve on test.matrix. expected message: 'getting cached data...'")
cacheSolve(test.matrix)