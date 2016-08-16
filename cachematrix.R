## makeCacheMatrix creates a special matrix that provides functions
## to get or set the matrix and get or set the inverse matrix.
## cacheSolve computes the inverse of the matrix if it has not been
## computed before or has changed. If the inverse has been computed
## and the matrix has not changed, then it retrieves a cached version
## of the inverse matrix.

## Returns functions to get or set the matrix and get or set the 
## inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverseX <- NULL
        set <- function(y) {
                x <<- y
                inverseX <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseX <<- inverse
        getInverse <- function() inverseX
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of the matrix, computing it if necessary 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseX <- x$getInverse()
        if (!is.null(inverseX)) {
                message("getting cached data")
                return (inverseX)
        }
        data <- x$get()
        inverseX <- solve(data, ...)
        x$setInverse(inverseX)
        inverseX
}
