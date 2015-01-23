## makeCacheMatrix  function creates a special "matrix" 
##object that can cache its inverse and return the funcitons as below:
##  setMatrix      set the value of a matrix
##  getMatrix      get the value of a matrix
##  setInverse     cahce the value 
##  getInverse     get the cahced value 



makeCacheMatrix <- function(x = matrix()) {
  # initially  set cached value  to NULL
  mCache <- NULL
  
  # set the matrix
  setMatrix <- function(y){
    # assgined  the matrix with new value
    x <<- y
    # flush the cache
    mCache <<- NULL
  }
  
  # get the stored martix
  getMatrix <- function() x
  
  # set the given argument
  setInverse <- function(solve) mCache <<- solve
  
  # get the cached value
  getInverse <- function() mCache
  
  # return a list. The elements  of the list are functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve calculates the inverse of a "special" matrix created with 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
  # get the value from makeCacheMatrix funtion
  inverseValue <- x$getInverse()
  
  # if the inverseValue is NULL, print a error message,
  # otherwise , it will return the cached Value
  if(is.null(inverseValue)){
    message("Didn't get cached data")
  }else{
    message("getting cached data")
    return(inverseValue)
  }
  
  # get the matrix and caclulate the inverse, then store it in the cache
  mData <-x$getMatrix()
  inverseValue <- solve(mData)
  x$setInverse(inverseValue)
  
  #return the  inverseValue
  inverseValue
 
}
