## This pair of functions computes the inverse of a matrix. It checks whether 
## the inverse has been computed previously, and if so, uses the cached value 
## instead of recomputing. 

## This function "makeCacheMatrix" creates a list of functions which set the 
## value of the matrix, get the value of the matrix, compute its inverse, and 
## get the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inverse) {
                inv <<- inverse
        }
        getinv <- function() {
            inv
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function calculates the inverse of the matrix x. However,
## it first uses the makeCacheMatrix functions to check whether the inverse ## has already been found. If so, it uses the cached inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
