## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
  
}


# The following function calculates the inverse of the matrix created with the
# makeCacheMatrix function. If the inverse has already been calculated, it gets
# the inverse from cache; othewise, it calculates the inverse, sets it to cache
# via the 'setinverse' function, and returns the inverse.


cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
  ## Return a matrix that is the inverse of 'x'

}
