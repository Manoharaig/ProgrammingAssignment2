## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                     # Initializing the inverse matrix to NULL
  set <- function(y) {          # function to set the a new value for the underlying matrix, We use <<- operator to set the value of x
    x <<- y                     # and inverse because we want to modify x and inverse defined in the closed environment        
    inv <<- NULL
  }
  get <- function() x           # getting the matrix
  setInverse <- function(inverse) inv <<- inverse       # set the inverse of matrix
  getInverse <- function() inv                          # Get the inverse of matrix
  list(set = set,                                      
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
          if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
          }
          mat <- x$get()
          inv <- solve(mat, ...)
          x$setInverse(inv)
          inv
        }

}
