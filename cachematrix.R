
## makeCacheMatrix() creates a special matrix object that can cache its inverse.
## the object is actually a list that allows you to set or get the matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set(y) allows the value of the matrix to be set to y
  ## Resets any previously calculated inverse when changed in this way
  set <- function(y) {
      x<<-y
      inv<<- NULL
  }
  ##get() gets the stored matrix
  get <- function() x
  ## setinverse() caches a calculated inverse value (this fx is used by cacheSolve)
  setinverse <-function(inverse) inv <<- inverse
  ##getinverse() retrieves the cached inverse
  getinverse <-function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve computes the inverse of a matrix made by makeCacheMatrix
## if there's a cached inverse, cacheSolve retrieves the inverse instead

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## Checks if there is a cached inverse and returns it if present
  if(!is.null(inv)) {
    message("Retrieving cached inverse")
    return(inv)
  }
  ## otherwise solves the inverse and caches it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
