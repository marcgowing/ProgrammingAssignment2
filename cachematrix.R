## Programming Assignment 2: "Caching the Inverse of a Matrix"
##
## This code is optimized to avoid unecessary recomputation of a matrix's
## inverse, which can be an expensive operation for large matrices. A special
## "matrix" object is used, that can cache its inverse.
##
## NOTE: The matrix is assumed to be square (n x n), and invertible.
##
## References:
## [1] This code is largely based off the "Caching the Mean of a Vector"
## example from the coursera webpage: "R Programming, Programming Assignment 2",
## https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions



## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.
##
## Usage:
##      m <- matrix( c(3,0,0,3), 2, 2 )     # create a (2x2) matrix m
##      x <- makeCacheMatrix(m)             # create a cache matrix x, using m
##      x$get()                             # get the matrix x
##      x$set(m)                            # resets the matrix x, clears cache

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)

}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.
##
## Usage:
##      cacheSolve(x)


cacheSolve <- function(x, ...) {

        inverse <- x$getinv()
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
