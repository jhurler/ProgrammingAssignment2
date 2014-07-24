## Below are two functions that are used to create a special
## object that stores a numeric matrix and caches its inverse.

## The first function, makeCacheMatrix, creates a special object,
## which is really a list containing four functions to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of its inverse
##   4. get the value of its inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(cachedMatrix = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
		## the <<- operator assigns a value to an object in
		## an environment different from the current environment.
                cachedMatrix  <<- y
                cachedInverse <<- NULL
        }
        get <- function() cachedMatrix
        setinverse <- function(newInverse) cachedInverse <<- newInverse
        getinverse <- function() cachedInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve - This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix has not
## changed), then cacheSolve retrieves the inverse from its cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## if X is a square invertible matrix,
	## then solve(X) returns its inverse.
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        newinverse <- solve(data, ...)
        x$setinverse(newinverse)
        newinverse
}
