## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function sets up the object that will hold the cached matrix:
## -- x is by default a blank matrix, but it can also be a specified matrix that
##    must be solvable
## -- set is a function that sets the matrix and its inverse
## -- get is a function that returns the matrix and its inverse
## -- setinverse will compute the inverse using solve()
## -- getinverse will return the cached inverse of the matrix
## what is returned is a list of functions to access both the matrix itself
## and also the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## This function will solve the inverse of the matrix and update the info in
## the makeCacheMatrix object which holds the matrix itself and also its inverse
## -- If the inverse has not been computed yet (i.e., inverse is NULL), then the 
##    function will compute the inverse and call setinverse from makeCacheMatrix
##    to change the value from NULL to the inverse of the function
## -- If the inverse has already been cached, then the function will return the
##    cached value of the inverse using getinverse in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
