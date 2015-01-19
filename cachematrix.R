## This file contains two functions:
##
## makeCacheMatrix() - which acts as a constructor for a "special" matrix object. It returns a list of functions for 
## setting and getting parts of a "special" matrix object.
##
## cacheSolve() - which calculates the inverse of a "special" matrix x, unless it already has been cached, in which 
## case the cached inverse is returned. 

###################

## makeCacheMatrix() acts as a constructor for a "special" matrix object. A "special" matrix object stores 
## both the matrix and its inverse.  The function makeCacheMatrix() provides the functions set() and setinverse() for ## creating a "special" matrix object from the "ordinary" matrix object x passed to it, and the functions get() and 
## getinverse() for accessing the "ordinary" parts of a "special" matrix object.

makeCacheMatrix <- function(x = matrix()) {

        ## Returns a list of constructor functions 

        inverseX <- NULL  # NULL the inverse whenever a new special matrix is created. This ensures that
                          # the stored inverse is current and relevant without invoking set().

        # Sets and Gets

        set <- function(y) {
                x <<- y
                inverseX <<- NULL
        }
        get <- function() x
        setinverse <- function(invmat) inverseX <<- invmat
        getinverse <- function() inverseX
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() returns the cached (stored) inverse of a "special" matrix x, if it exists. If nothing has been 
## cached, cacheSolve() calculates the inverse directly using the R language's native solve() function.

cacheSolve <- function(x, ...) {

        ## Returns a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        storedX <- x$get()
        m <- solve(storedX, ...)
        x$setinverse(m)
        m
}

## A bit of test code - please ignore
##  testmatrix <- matrix(c(3,5,1,2),2,2)
##  tm <- makeCacheMatrix(testmatrix)
##  tminv1 <- cacheSolve(tm)
##  tminv2 <- cacheSolve(tm)
##  testmatrix <- matrix(c(4,7,1,2),2,2)
##  tm <- makeCacheMatrix(testmatrix)
##  tminv3 <- cacheSolve(tm)

