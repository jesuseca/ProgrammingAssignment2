## The following functions allow us to calculate the inverse of a square matrix and efficiently 
## use resources by storing the inverse matrix in memory and invoke it whenever necessary, as well as
## recalculate its inverse if the square matrix has changed.


## Create a special matrix, which manages its original and inverse values by storing both values in memory

makeCacheMatrix <- function(x = matrix()) {
    solx <- NULL
    set <- function(y) {
        x <<- y
        solx <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) solx <<- solve
    getsolve <- function() solx
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

 
## Return the inverse matrix of x, if it already exists, takes it from memory otherwise calculate it.

cacheSolve <- function(x, ...) {

    solx <- x$getsolve()
    if(!is.null(solx)) {
        message("getting cached data")
        return(solx)
    }
    data <- x$get()
    solx <- solve(data, ...)
    x$setsolve(solx)
    solx
}
