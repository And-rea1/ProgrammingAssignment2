#### WEEK 3 Programming Assignment:

## The two functions defined below have the following attributes:
#1: create a matrix that can cache its inverse
#2: Comupute the inverse of the matrix created with the past function,
#and retrieve the inverse from the cache if the inverse was calculated allready

## The makeCacheMatrix function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function comuputes the inverse of the matrix created with the makeCacheMatrix function,
# and retrieve the inverse from the cache if the inverse was calculated allready

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
