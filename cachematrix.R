# Matrix inversion can be a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it 
# repeatedly. The following two functions are used to cache the inverse
# of a matrix

## makeCacheMatrix creates a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(minv) inv <<- minv
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve returns the inverse of a matrix.
# It assumes that the matrix is square and invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
