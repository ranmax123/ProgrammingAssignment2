## Purpose: The purpose of this function is to store the cached matrix in a variable m,
## and return a list containing the set of functions that we can invoke on the 
## cached matrix. The function 'set' sets the matrix, 'get' gets the matrix, and the 
## functions 'setinverse' and 'getinverse' sets and gets the inverse of the matrix 
## respectively
## Input: a matrix
## Return: a list containing functions that can be invoked on the cached matrix
## Test script: 
## data =matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE, 
## dimnames=list(c("row1", "row2"), c("C.1", "C.2")))
##
## makeCacheMatrix(data)$get()
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    ## set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the matrix
    get <- function() x
    
    ## set inverse
    setinverse <- function(inverse) m <<- inverse
    
    ## get inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Purpose: The purpose of this function is to find inverse of the matrix, and
## return the inverse. This function first checks if the inverse is already computed, if yes
## then gets the data from cache, else computes it afresh.
## Input: an object of makeCacheMatrix
## Return: the inverse of the matrix
## Test script: 
## data =matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE, 
## dimnames=list(c("row1", "row2"), c("C.1", "C.2")))
##
## cachematrix <- makeCacheMatrix(data)
## cacheSolve(cachematrix)
##
## cacheSolve(cachematrix)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## get the matrix
    data <- x$get()
    
    ## compute inverse
    m <- solve(data, ...)
    
    ## set the inverse
    x$setinverse(m)
    
    m
}
