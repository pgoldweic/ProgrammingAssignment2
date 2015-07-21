## These two functions work very similarly to how the example functions for this programming assignment
## (that is, makeVector and cacheMean) work

## makeCacheMatrix creates a special "matrix" which is really a list containing functions to:
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse
## 4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i<<- inv
  getinverse <- function() i
  list(set= set, get = get, setinverse= setinverse, getinverse= getinverse)
}


## cacheSolve calculates the inverse of a special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated, and if so, returns the
## inverse value from the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix - using solve(x)- and sets the value of the cached inverse via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
      message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
