## Caching the inverse of a matrix by a pair of functions

## This function creates a special "matrix" object, which is in fact a list to a set of functions

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(invmat) i <<- invmat
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}    
      


## This function computes the inverse of the special "matrix" object

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting the cached inverse matrix")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
