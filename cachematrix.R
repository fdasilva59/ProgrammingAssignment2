## Coursera assignment : Week 3 
## This script provides a pair of functions that cache the inverse of a matrix
##
## Usage example :
##
##      # Source the script before use
##      > m <- matrix(runif(9),c(3,3))    # create a test matrix (inversible)
##      > cm <- makeCacheMatrix()         # make a special matrix
##      > cm$set(m)                       # set the matrix in the special matrix
##      > cm$get()                        # check it's there
##      > cm$getminv()                    # check the cache is empty 
##      NULL
##      > cacheSolve(cm)                  # compute/store in cache the inverse
##      storing inverse of matrix in cache
##      [,1]       [,2]       [,3]
##      [1,]  2.0867626  1.0407853 -3.2144860
##      [2,] -0.5305211 -0.8478860  2.4288228
##      [3,] -0.8992290  0.7062142  0.9622029
##      > cacheSolve(cm)                  # retrieve the inverse from cache
##      getting cached inverse of matrix
##      [,1]       [,2]       [,3]
##      [1,]  2.0867626  1.0407853 -3.2144860
##      [2,] -0.5305211 -0.8478860  2.4288228
##      [3,] -0.8992290  0.7062142  0.9622029


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    # Initialize the cache variable for the inverse of the matrix (empty)
    minv <- NULL
    
    # Define a function to set the matrix 
    set <- function(y) {
        x <<- y
        minv  <<- NULL  #  erase cache if defined
    }
    
    # Define a function to get the matrix 
    get <- function() x
    
    # Define a function to set the inverse of the matrix 
    setminv <- function(inv_matrix) minv <<- inv_matrix
    
    # Define a function to set the inverse of the matrix 
    getminv <- function() minv 
 
    list(set = set, get = get, setminv = setminv, getminv = getminv)
       
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {

    # Check if the inverse of the matrix exist in the cache
    minv <- x$getminv()
    if(!is.null(minv)) {
        # OK the value exist : return the cached value
        message("getting cached inverse of matrix")
        return(minv)
    }
    
    # Need to calculate the inverse of the matrix and put it in cache
    data <- x$get()
    matinv <- solve(data, ...)
    message("storing inverse of matrix in cache")
    x$setminv(matinv)
    
    ## Return a matrix that is the inverse of 'x'
    matinv
    
}
