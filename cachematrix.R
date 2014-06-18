##The two functions below  are used to create a object that stores a matrix and cache's its inverse

## The first function 'makeCacheMatrix' creates a "matrix".

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setM <- function(y) {
    x <<- y
    m <<- NULL
  }
  getM <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setM = setM, getM = getM,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function calculates the inverse of the matrix created with the above function.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getM()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
