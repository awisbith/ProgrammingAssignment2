## Create a cachable version of a matrix that allow the inverse to be
## calculated and cached.


## Takes an argument of x that is a matrix and returns a list
## of functions that implement caching and inversing of that matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  getInverse <- function() i
  setInverse <- function(y) i <<- y
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}


## This function calculates the inverse of a matrix. When the inverse
## is caluculated it is cached.  If the inverse is requested again
## the cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
