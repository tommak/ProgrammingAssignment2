## Below are two functions that are used to create a special object that stores
## a matrix and caches corresponding inverse matrix  


## The function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list (set = set, get = get, 
          setinv = setinv, getinv = getinv)
}


## The function computes the inverse of the special "matrix" returned by function
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.
## Input matrix is assumed invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message ("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
