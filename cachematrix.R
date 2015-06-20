## The following functions are my solution for an assignment from 
## a Coursera course called R programming. It uses lexical scoping rules 
## and cache funtions to cache time-consuming computations such as matrix inversion. 


## The following function creates a special "matrix" object 
## that can cache its inverse. It returns a list containing functions to
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
## This list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix

        m <- NULL ##to store the result of the inverse of the matrix

        setmatrix <- function(y){
                x <<- y
                m <<- NULL
        }

        getmatrix <- function() x          ## return the input matrix
        setinv <- function(inv) m <<- inv  ## set the inversed matrix
        getinv <- function() m             ## return the inversed matrix

        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)

}


## The following function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then the `cachesolve` 
## should retrieve the inverse from the cache.
 
cacheSolve <- function(x, ...) {
        ##x is the output of makeCacheMatrix()

        m <- x$getinv() ##get the inversed matrix

        ## if an inverse has been calculated return the calculated inversion
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ##otherwise, get the matrix object and caculate the inverse of the matrix
        mat <- x$getmatrix()
        m <- solve(mat, ...)
        ## sets the value of the inverse in the cache
        x$setinv(m)
        m ##return the inverse of the matrix input to makeCacheMatrix()

}


