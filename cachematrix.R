## This function is capable of caching the inverse of a matrix
## using two functions makeCacheMatrix and cacheSolve as below 

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)    
}


## This function compute the inverse of the special matrix
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinv(inverse)
    inverse
}
