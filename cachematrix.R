## This a pair of functions caches the inverse of a matrix.

## makeCacheMatrix creates a list of functions that sets the input matrix, gets the input matrix, sets the inverse matrix, and gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # Default value for inverse matrix i
  i <- NULL
  # set the input matrix
  set <- function(y = matrix()){
    x <<- y
    i <<- NULL
  }
  # get the input matrix
  get <- function() x
  # set the inverse matrix
  setinverse <- function(inverse) i <<-inverse
  # get the inverse matrix
  getinverse <- function() i
  # return the list of functions that would be used for cacheSolve function below
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of a matrix using the list created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}