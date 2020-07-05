## Author: Douglas Mendes

## Functions to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

  myInv <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    myInv <<- NULL
  }
  
  get <- function() {
    m
  }
  
  setInverse <- function(inverse) {
    myInv <<- inverse
  }
  
  getInverse <- function() {
    myInv
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}





cacheSolve <- function(x, ...) {

  cachedData <- x$getInverse()
  
  if( !is.null(cachedData) ) {
    return(cachedData)
  }
  
  data <- x$get()
  
  cachedData <- solve(data) %*% data
  
  x$setInverse(cachedData)
  
  cachedData
  
}
