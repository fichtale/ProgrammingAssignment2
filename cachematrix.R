## Pair of functions that cache the inverse of a matrix.

## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ##Initialize inverse property.
  i <- NULL

  ##Method to set matrix.
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ##Method to get matrix.
  get <- function () {
    ##Return matrix.
    x
  }
  
  ##Method to set inverse of matrix.
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ##Method to get inverse of matrix.
  getInverse <- function() {
    ##Return inverse.
    i
  }
  
  ##Return list of methods.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##Compute inverse of matrix returned by makeCacheMatrix above. If inverse has already been computed and not changed, then cachesolve will retrieve inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'.
  x <- x$getInverse()
  
  ##Just return inverse if already set.
  if (!is.null(x)) {
    message("getting cached data")
    return(x)
  }
  
  ##Get matrix from object.
  data1 <- x$get()
  
  ##Calculate inverse using matrix multiplication.
  x <- solve(data1) %*% data1
  
  ##Set inverse to the object.
  x$setInverse(x)
}
