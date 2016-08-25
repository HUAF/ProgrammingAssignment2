## makeCacheMatrix create a matrix object and its methods
## it can store a matrix, its inverse and the setters and getters
## cacheSolve takes a makeCacheMatrix object as argument,
## returns the cached inverse matrix if it exists or 
## computes the inverse matrix and cache it.


## makeCacheMatrix takes an invertible matrix as argument, caching the
## matrix and binding setters and getters for the matrix and the inverted matrix

makeCacheMatrix <- function(invertibleMatrix = matrix()) {
    ## set invertedMatrix to NULL
    inverseMatrix <- NULL
    ## sets the invertibleMatrix value and the invertedMatrix to NULL 
    ## (if the value of the matrix changes its inverted matrix changes too)
    set <- function(y) {
        invertibleMatrix <<- y
        inverseMatrix <<- NULL
    }
    ## return the invertible matrix
    get <- function() invertibleMatrix
    ## set the inverse matrix
    setInverse <- function(solve) inverseMatrix <<- solve
    ## returns the inverse matrix
    getInverse <- function() inverseMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(invertibleMatrix, ...) {
        ## Return a matrix that is the inverse of 'invertibleMatrix'
    inverseMatrix <- invertibleMatrix$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached inverse matrix")
        return(inverseMatrix)
    }
    data <-invertibleMatrix$get()
    inverseMatrix <- solve(data)
    invertibleMatrix$setInverse(inverseMatrix)
    inverseMatrix
}
