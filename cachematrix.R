
## The functions below can be used to create a matrix that can cache its inverse. 
## Calculating the inverse of a matrix is costly, so it can be useful to cache the
## inverse once its calculated and to retrieve it directly from memory later, instead of
## calculating it from scratch every time. 


## This function creates a special "matrix" object that can cache its inverse.
## The inverse of the matrix is contained in the object "inv". 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(x, ...)
    x$setinv(inv)
    inv
}
