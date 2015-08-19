makeCacheMatrix <- function(x = matrix()) {
  ##holds the cached value. Initially holds the null value
  cacheVal <- NULL
  ## store a matrix
  setMatrix <- function(newVal) {
    x <<- newVal
    cacheVal <<- NULL
  }
  ## retrieve the stored matrix
  getMatrix <- function() {
    x 
  }
  ## store the inverse matrix
  setinverse <- function(inverse){
    
  cacheVal <<- inverse
  }
  
  ## get the cahed matrix
  getinverse <- function() {
    cacheVal
  }
  ## return list of functions
  list(setMatrix=setMatrix, getMatrix=getMatrix, setinverse=setinverse, getinverse=getinverse)
}

## this functions calculates the inverse of matrix and cache it
cacheSolve <- function(x, ...) {
  
  ## get cached value
  cacheVal <- x$getinverse()
  ## if cached values exists return it
  if(!is.null(cacheVal)) {
    message("getting cached data.")
    return(cacheVal)
  }
  ## otherwise get the the matrix and calculate the inverse and cache the 
  data <- x$getMatrix()
  
  cacheVal <- solve(data)
  x$setinverse(cacheVal)
  
  ## return the inverse
  
  cacheVal
}