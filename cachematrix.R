#Matrix inversion is usually a costly computation and their may be some benefit to caching the 
#inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
#inversion that we will not discuss here). Your assignment is to write a pair of functions that 
#cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.It has functions to set the value of the 
#matrix, get the value of the matrix, set the value of inverse of the matrix and to get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data.")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
