## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function implements the chace mechanism like in example

makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)


}


## Write a short comment describing this function
## Current function inspects if the chace contains the invers for matrix "x"
## if value exists inchache it is returned else the inverse matrix is calculated
## via "solve()" function, its result is stored in chace and then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
