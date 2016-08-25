## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes an invertible matrix as argument, caching the
## matrix and binding setters and getters for the matrix and the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    ## set invertedMatrix to NULL
    m <- NULL
    ## sets the invertibleMatrix value and the invertedMatrix to NULL 
    ## (if the value of the matrix changes its inverted matrix changes too)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## getter
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <-x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
