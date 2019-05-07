## makeCacheMatrix returns a list of functions able to store a
## matrix and also its inverse, if it has been computed by the
## function cacheSolve. This latter function only computes the
## inverse if it hasn't already been computed and cached

## Returns a list of functions to access the matrix x and
## also its inverse once the inverse has been computed using
## cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Computes the inverse of a matrix produced by makeCahceMatrix
## if it has not already been computed and cached
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
