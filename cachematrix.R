
## These functions combined allow for caching the Inverse of a Matrix.
## This calculation can be expensive so there can be benefits to only performing
## the calculation once.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
	  set <- function(y) {
		x <<- y
		inv <<- NULL
	  }
	  get <- function() x
	  setInverse <- function(inverse) inv <<- inverse
	  getInverse <- function() inv
	  list(set = set,
		   get = get,
		   setInverse = setInverse,
		   getInverse = getInverse)
}


# Computes the inverse of the matrix created by makeCacheMatrix. The calculation
# will only occur if there is no cached value.

cacheSolve <- function(x, ...) {
	  inv <- x$getInverse()
	  if (!is.null(inv)) {
		return(inv)
	  }
	  mat <- x$get()
	  message("solving...")
	  inv <- solve(mat, ...)
	  x$setInverse(inv)
	  inv
}
