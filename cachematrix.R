##create a matrix named X and return its inverse named inverse_x
makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse_x <<- inv
  getinv <- function() inverse_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##Compute the inverse of the special "matrix" returned by makeCacheMatrix above, or retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inverse_x <- x$getinv()
  if(!is.null(inverse_x)) {
    message("getting cached data")
    return(inverse_x)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setivn(inverse_x)
  inverse_x
}
