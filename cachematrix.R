## The two functions defined in this file create the framework for cacheing the
## inverse of a matrix to save computation time.

## makeCacheMatrix creates a matrix-type object along with a storage variable for the
## inverse of the input matrix.
## x    matrix      input matrix
## m    matrix      storage for inverse of x (set to NULL / later redefined to matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks the input object, x, which should be a matrix-type object
## previously defined using makeCacheMatrix.
## x    matrix    matrix-type object (output of makeCacheMatrix)
##
## The function checks to see if m is NULL or populated.  If NULL, the function
## proceeds to calculate the inverse of the input matrix.  If not NULL, then the
## inverse is assumed to be stored, and the stored value is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
