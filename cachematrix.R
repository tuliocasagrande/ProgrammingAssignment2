## These functions manipulates a matrix with a cached version of its inverse

## makeCacheMatrix creates a special "matrix", with the following features:
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse
## 4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve returns a matrix that is the inverse of 'x'. If there is a cached
## inverse, this function skips the computation and returns the cache. Otherwise,
## it calculates the inverse, sets in the cache and returns the value.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
