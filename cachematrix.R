## makeCacheMatrix is a function that reads in the matrix whose inverse will be computed. 
## It returns a list containing four functions:  
## set: Set the values in the matrix
## get: Get the values in the matrix 
## setinverse: Set the values in the inverse matrix 
## getinverse: Get the values in the inverse matrix 
##
## cacheSolve is a function that calculates the inverse of the matrix provided in makeCacheMatrix.
## It checks to see if the inverse of the matrix has already been calculated. 
## If it has, it gets the inverse matrix from cache without completing the computation.
# If the inverse has not already been computed, it calculates and returns it. 


## returns a list containing four functions to compute the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

## returns inverse of matrix; grabs it from cache if it has already been computed. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}