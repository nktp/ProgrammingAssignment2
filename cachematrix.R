## Compute the inverse of a square matrix using cache where previously calculated.

## Creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
        x <<- y
        m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Computes the inverse of matrix returned by makeCacheMatrix. If previously calculated, retrieves inverse from cache.

cacheSolve <- function(x, ...) {

  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
