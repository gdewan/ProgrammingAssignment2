## Make repeated matrix inversion fast, by caching the inverse after the first
## call.


## This function creates a new "matrix" that is capable of caching its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  get <- function() x
  set <- function(m) {
    x <<- m
    inverse_matrix <<- NULL
  }
  getInverse <- function () inverse_matrix
  setInverse <- function(inverse) {
    inverse_matrix <<- inverse
  }
  list(get = get, 
       set = set, 
       getInverse = getInverse, 
       setInverse = setInverse)
}


## This function efficiently inverts a matrix for repeated invocations for
## the same matrix, by returning a cached copy after the first call.

cacheSolve <- function(x, ...) {
  inverse_matrix = x$getInverse()
  if (is.null(inverse_matrix)) {
    m = x$get()
    inverse_matrix = solve(m)
    x$setInverse(inverse_matrix)
  } 
  inverse_matrix  
}
