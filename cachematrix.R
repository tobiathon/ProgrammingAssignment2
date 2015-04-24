## These function speed up the process where a matrix 
## inverse needs to be calculated many times

## Returns matrix with caching ability to speed process

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solution) m <<- solution
    getmatrix <- function() m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## this returns the inverse of a stored matrix, if cache is empty,
## calculates inverse matrix and stores it in cache

cacheSolve <- function(x, ...) {
        solution <- x$getmatrix()
        if(!is.null(solution)){
            message("getting cached data")
            return(solution)
        }
        data <- x$get()
        solution <- solve(data, ...)
        x$setmatrix(solution)
        solution
}
