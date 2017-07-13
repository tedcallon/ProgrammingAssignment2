## Following functions create a matrix that can cache its inverse
## and then provides a function that calculates the inverse of the
## matrix if the inverse hasn't already been cached, else returns the 
## cached value

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of the object returned by makeCacheMatrix. If
## the inverse has already been calculated and the matrix has not been
## changed it returns the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        return(inv) 
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
