## two nested functions one to create a matrix and the other to cache it's inverse 
## These functions creates a "matrix" and then cache its inverse.

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



##This first function (above) computed the inverse of the  "matrix" created under 
## the makeCacheMatrix object above. If the inverse was calculated and remains 
## unchanged then the inverse can be retrieved from the cache using...

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv)) {
    message("retreiving cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
