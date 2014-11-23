## These two functions are able to calculate value of inverted matrix and cache it.

makeCacheMatrix <- function(x = matrix()) {
## This function makes a "matrix" object, a list that is able to set and get values of
## both source and inverted matrices
  
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



cacheSolve <- function(x, ...) {
  ## This function inverts the "matrix" obtained by earlier function.
  ## If it's been calculated before, then function gets cached data and returns it.
    
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
