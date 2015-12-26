## This pair of functions creates a special "matrix" object that can cache its inverse, and then either inverts the special "matrix", or returns the inverted "matrix" from the cache.

## Given a matrix this function will create a special "matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## If the special "matrix" object has not yet been inverted this function will do so and return the results. Otherwise it wil will retrieve the inverse of the special "matrix" object from the cache and return the results.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
