## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates the object which has field called inverse, for storting matrix inverse.
## This function does the creation and initialization of the makeCacheMatrix object.
## The inverse is reset to NULL each time the initialization happens.
makeCacheMatrix <- function(x = matrix()) {
    #'inverse' is the variable used to store the inverse of the matrix object.
    #'initialise the "inverse" to NULL.
    inverse <- NULL
    
    #set function initializes the matrix with the specified values.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    #get function returns the value.
    get <- function() x
    
    #setinverse function calculates the inverse.
    setinverse <- function(solve) inverse <<- solve
    
    #returns the inverse.
    getinverse <- function() inverse
    
    #create the list with all the parameters.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## This function returns the inverse of the matrix from the cache , if already computed.
## Else it computes the inverse and returns the inverse.
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
    
}
