## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Object to cache inverse into
  cachedinverse <- NULL
  
  ##Return the base matrix
  get <- function() x
  
  ##Method to change the matrix
  set <- function(y){
    x <<- y
    ##Clear the cached version
    cachedinverse <<- NULL
  }
  
  ##Method to get the inverse. Returns the cached version  
  getInverse <- function() cachedinverse
  
  ##Method to set the cached val
  setInverse <- function(y){
    cachedinverse <<- y
  }  
  
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}


## This function computes the inverse of the matrix object passed. 

cacheSolve <- function(x, ...) {
  
  ##First read the cached version
  inverse <- x$getInverse()
  
  ##If not null return it
  if(!is.null(inverse)){
    return (inverse);
  }
  print("Not cached so need to solve matrix")
  
  ##If null then calculate the inverted matrix and cache
  inverse <- solve(x$get())
  
  ##Cache the inverse resultc <
  x$setInverse(inverse)
  
  ##Return the inverse
  inverse
}
