## With these function we can speed up the process, especially in loops
## when the matrix doesn't change, inverse of matrix doesn't need to be calculated
## repeteadly

## makeCacheMatrix is a list of four function:
## with setmatrix the matrix can be changed
## getmatrix returns the matrix (if changed with setmatrix, the changed one)
## with setinv we can set the inverse of the matrix
## getinv returns the inverse of matrix
##(if setinv was used, then the inverse given by setinv will be returned )

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                setmatrix <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                getmatrix <- function() x
                setinv <- function(solve) inv <<- solve
                getinv <- function() inv
                list(setmatrix = setmatrix, getmatrix = getmatrix,
                     setinv = setinv,
                     getinv = getinv)
        }      



## cacheSolve will look whether there was inverse stored in makeCacheMatrix.
## if yes, it returns the inverse stored there (with 'getinv')
## if we don't have stored inverse, then cacheSolve will calculate
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
