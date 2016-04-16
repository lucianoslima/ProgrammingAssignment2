##These two functions are used to create a special object that 
#compute and cache the inverse of a matrix.

## First function, makeCacheMatrix().
##Create a list containing functions to be used as the input to cacheSolve()
##The list contain functions to:
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## assign NULL value to inverse, initialize the inverse variable
  inverse <- NULL
  ##create the function set  
  set <- function(y) {
    ## assign y value to x in cache
    x <<- y
    ## assign NULL value to inverse in cache
    inverse <<- NULL
  }
  ##create the function get  
  get <- function() x
  ##create the function setinverse    
  setinverse <- function(solve) inverse <<- solve
  ##create the function getinverse    
  getinverse <- function() inverse
  ##create a list with the functions to be used   
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Second function, cacheSolve().
## Return the inverse of the matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  ## x is the output of the makeCacheMatrix(), a list with 4 functions.
  ## run getinverse function to inverse variable 
  inverse <- x$getinverse()
  ##check if the inverse matrix has already been calculated
  if(!is.null(inverse)) {
    ##if yes get the inverse matrix from the cache and skip computation
    message("getting cached data")
    return(inverse)
  }
  ##otherwise calculates the inverse matrix
  ## get the original matrix x
  data <- x$get()
  ##computate the inverse matrix of x
  inverse <- solve(data, ...)
  ##set the inverse matrix in the cache, using setinverse function
  x$setinverse(inverse)
  # return the inverse matrix
  inverse
}
