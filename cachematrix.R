## The functions makeCacheMatrix and cacheSolve cache the inverse
## of a square matrix. For these functions, assume that the matrix
## supplied is always invertible.
##
## Examples
## source("cachematrix.R")
## m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## m$get() # Returns original matrix
## cacheSolve(m) # Computers, caches, and returns matrix inverse
## m$getinverse() # Returns matrix inverse
## cacheSolve(m) # Returns cached matrix inverse
##
## m$set(matrix(c(2,3,4,5), nrow=2, ncol=2)) # Modify existing matrix
## cacheSolve(m) # Computers, caches, and returns new matrix inverse
## m$get() # Returns new matrix
## m$getinverse() # Returns new matrix inverse

## Creates a special "matrix" object that can cache the inverse of 
## a matrix, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(z) inverse <<- z
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has
## not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
