## These functions cache the inverse of a matrix
## By caching the inverse matrix, if the matrix is needed
## again, the computation does not need to be re-performed

## Creates a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
        x <<- y
        j <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) j <<- solve
    getinverse <- function() j
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has been calculated, then this function will
## retrieve the inverse matrix from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    j <- x$getinverse()
    if(!is.null(j)) {
        message("getting cached inverse matrix")
        return(j)
    }
    data <- x$get()
    j <- solve(data, ...)
    x$setinverse(j)
    j        
}
