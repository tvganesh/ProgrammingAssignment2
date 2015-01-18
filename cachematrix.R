## The R functions makeCacheMatrix() and CacheSolve() return the inverse of a 
## matrix if not already found in the cache
## makeCacheMatrix() - Takes as input a matrix and creates 3 functions
## setinverse() which sets the matrix inverse
## getinverse() returns the inverse
## and sets a list of functions

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse to NULL
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  # Store the inverse of a matrix in the inverse variable
  setinverse <- function(matinverse) inverse <<- matinverse
  
  # Return the cached inverse
  getinverse <- function() inverse
  
  # Return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes as input an environment created by makeCacheMatrix
## Given an input matrix checks if the inverse is cached. If it is, then return the cached inverse
## Else call the function solve(mat) and return the inverse

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  # If inverse is cached and not NULL then return this
  if(!is.null(inverse)) {
    message("Getting inverse of matrix")
    return(inverse)
  }
  
  # If inverse not cached then call the function solve to compute inverse
  data <- x$get()
  inverse <- solve(data, ...)
  
  # Store the inverse by calling x$setinverse
  x$setinverse(inverse)
  
  # Return the inverse
  inverse
}
