## The functions will cache the inverse of a matrix if a cache
## does not already exist

## This function will cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    solveMatrix <- NULL
    set <- function(y) {
        x <<- y
        solveMatrix <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) solveMatrix <<- solve
    getsolve <- function() solveMatrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function will check for a cache of the inverse if a matrix and
## if none exists pass the matrix to makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    solveMatrix <- x$getsolve()
    if(!is.null(solveMatrix)){
        message("getting cached data")
        return(solveMatrix)
    }
    data <- x$get()
    solveMatrix <- solve(data)
    x$setsolve(solveMatrix)
    solveMatrix
}
