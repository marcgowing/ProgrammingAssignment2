## This code is an optimization, that avoids unecessary recomputation of
## a matrix's inverse.
## A special "matrix" object is created, that can cache its inverse
## NOTE: The matrix is assumed to be square (n x n), and invertible

## This function creates a special "matrix" object that can cache its inverse.
## Example usage:
##         m <- matrix( c(3,0,0,3), 2, 2 )		# create a matrix m
##         x <- makeCacheMatrix(m)			# create a cache matrix x, using m
##         x$get()											# get the matrix x

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


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.
## Example usage:
##         cacheSolve(x)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
