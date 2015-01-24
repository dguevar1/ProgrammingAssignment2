## These functions cache the inverse of a matrix since 
## solve can be computationally expensive.

## Make a 'special' matrix that is used by the cacheSolve 
## function to check if the inverse of this matrix has 
## already been calculated.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the solve
  setsolve <- function(solve) m <<- solve
  # get the value of the solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Checks the cache to see if the inverse of 'x' has been
## caculated. If so, it retursn it. Otherwise, it proceeds
## to solve the inverse and save it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
