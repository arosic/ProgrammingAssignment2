# These functions cache the inverse of a matrix, so that it does
# not need to be computed repeatedly. If the inverse has already been
# cached, it is retrieved; otherwise, it is calculated and set in the cache.

# The following function creates a special matrix object that can cache
# its inverse. It does this by creating a list that contains four functions:
# 1. 'set' which sets the matrix
# 2. 'get' which gets the matrix
# 3. 'setinverse' which sets the inverse of the matrix (to cache)
# 4. 'getinverse' which gets the inverse of the matrix (from cache)

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

# The following function calculates the inverse of a given matrix. If the inverse
# has already been calculated, it retrieves the inverse from cache; othewise, it 
# calculates the inverse, sets it to cache via the 'setinverse' function, and 
# returns the inverse.

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
}
