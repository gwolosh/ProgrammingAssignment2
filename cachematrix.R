## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Four functions
#	1. set - cache initial values
#	2. get - get the matrix
#	3. setinvert - cache the inverted matrix
#	4. getinvert - get the cached inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) invert <<- solve
        getinvert <- function() invert
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)

}


## Write a short comment describing this function
# Check to see if the inverted matrix is cached. If it
# is cached, return the inverted matrix. If not, calculated
# the inverted matrix and cache it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getinvert()
        if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setinvert(invert)
        invert
}

