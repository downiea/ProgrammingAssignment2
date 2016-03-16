## Given the possible computational cost of matrix inversion, 
## this pair of functions allow for caching the inverse matrix.

## This function takes a matrix and returns a list of functions which
## allow us to store the initial matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMatrix) i <<- inverseMatrix
        getinverse <- function() i
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Takes the list created by the above function and returns the
## inverse matrix. It will return a cached version of the inverse
## matrix, or calculate, store and return the inverse as required.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
