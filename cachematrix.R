## This file contains 2 functions. makeCacheMatrix and cacheSolve to cache the inverse
## of a matrix alowing to save potential time-consuming computations.

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

    i <- NULL # intializes i, the inverse, to NULL (provides a default if cacheSolve has not yet been run)
  
    set <- function(y) { ## caching matrix passed as parameter y
    x <<- y  ## y, matrix, gets cached into x in a different environment. ## the inputted matrix so that cacheSolve can check whether it has changed ( within the set function)
    i <<- NULL ## sets the value of i to NULL in a diff. env. (the matrix inverse if used cacheSolve)
  }
  
    get <- function() x #gets value of input matrix, x
  
    setinverse <- function(i) i <<- solve(x) ##calculate inverse and stores in cache. x is the input matrix
  
    getinverse <- function() i ## gets inverse from cache
  
    list(set = set, get = get,  ##create a list to hold the 4 functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' matrix
  
  i <-  x$getinverse()# retrieves inverse from cache - checks whether the inverse is in the cache already
  
  if(!is.null(i) || identical(x$get(),x)) {    # if inverse was cached,AND, cached matrix 
    ## and input matrix are same, then return i, which is the cached inverse
    message("getting cached data") 
    ## this msg helps us telling that value is being retrieved from cache
    return(i)
  }
  
  #if inverse is not in the cache (i is NULL) or the input matrix has changed
  data <- x$get()# call get function to get the value of the "input" matrix
  i <- solve(data, ...)# compute the value of the inverse of the input matrix
  x$setinverse(i) # store inverse of input matrix in cache
  i  ##return inverse of input matrix
}
