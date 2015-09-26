## cachematrix.R
## Functions designed to cache the inverse of a matrix to avoid 
## recalculating each time it is needed.

## makeCacheMatrix: Store a matrix and its Inverse.
## Arguments: x = The matrix to be stored

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: Solve the matrix, using cached inverse if available
## Arguments: x = matrix to be inverted, must be created with 
##                the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
