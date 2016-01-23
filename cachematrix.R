## This file contains 2 functions. makeCacheMatrix and cacheSolve to cache the inverse
## of a matrix alowing to save computational time.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # intializes i to NULL (provides a default if cacheSolve has not yet been used)
  set <- function(y) { ## caching matrix passed as y
    x <<- y  ## caches the inputted matrix so that cacheSolve can check whether it has changed ( within the set function)
    i <<- NULL ## sets the value of i to NULL (the matrix inverse if used cacheSolve)
  }
  get <- function() x
  setinverse <- function(x) i <<- solve(x) ##calculate inverse
  getinverse <- function() i
  list(set = set, get = get,  ##create a list to hold the 4 functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <-  x$getinverse()# retrieve inverse from cache
  if(!is.null(i)) {# if inverse has been previously cached, then return i, which is the cached inverse
    message("getting cached data")
    return(i)
  }
  #if cache is empty
  data <- x$get()# call get function to get the value of the input matrix
  i <- solve(data, ...)# compute the value of the inverse of the input matrix
  x$setinverse(i)# call setinverse on the inverse to cache the inverse
  i  ##return inverse
}
