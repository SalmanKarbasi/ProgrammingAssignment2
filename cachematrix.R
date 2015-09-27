# This function create a special matrix that can cache its inverse and will be used by cacheSolve function

makeCacheMatrix <- function(x = numeric()) { # x is in the input matrix 
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve # solve function calculate the inverse of input matrix 
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


# cache solve caluclate the inverse of the calculated matrix with makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
