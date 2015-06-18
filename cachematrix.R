## cachematrix.R consists of two functions (makeCacheMatrix and cacheSolve) that
## use caching to minimize amount of computationally heavy inversing of a matrix.
## It will only solve the inverse matrix again, if the original matrix 
## has been changed since the previous calculation.

## makeCacheMatrix takes a square matrix as input and stores this.
## When makeCacheMatrix is assigned to an object, the object will contain
## the 4 functions 'set', 'get', 'setim' and 'getim'.
## These functions can then be used e.g. like [objectname]$get() to get stored matrix
makeCacheMatrix <- function(x = matrix()) {
        ## Clear cached inverse matrix:
    im <- NULL
        ## 'set' is used to store a new matrix 'x'. 
        ## And clear the cached inverse matrix 'im', because it would be outdated:
     set <- function(y) {
        x <<- y
        im <<- NULL
    }
        ## 'get' can be used to retrieve stored matrix 'x'.
    get <- function() x
        ## 'setim' is caching the inverse matrix in variable 'im'
    setim <- function(inversematrix) im <<- inversematrix
        ## 'getim' will retrieve the cached inverse matrix in 'im'.
    getim <- function() im
        ## The 4 functions above will be stored in the function makeCacheMatrix.
        ## This is done via the function list(). 
    list(set = set, get = get,
         setim = setim,
         getim = getim)
}

## cacheSolve returns an inverse matrix of the invertible matrix stored in 'x'
## But it will first check if there is an inverse matrix stored in cache.
## This will prevent unnecessary re-calculations of the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Get the cached inverse matrix (im), if possible.
    im <- x$getim()
        ## Cache is empty, then the 'if'-statement resolves to FALSE. 
        ## When false it will skip the 'if'-clause. 
        ## Else function will return inverse matrix from cache:
    if(!is.null(im)){
        message("Getting cached data...")
        return(im)
    }
        ## Retrieve the matrix from object 'x' that needs inverting:
    data <- x$get()
        ## Actual calculation of the inverse matrix:
    im <- solve(data, ...)
        ## Cache the inverse matrix for future use in 'x':
    x$setim(im)
        ## Return the inverse matrix:
    im
}
