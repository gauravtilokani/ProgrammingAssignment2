## The functions compute the inverse of the matrix and store it in cache. If it is already stored in cache,
## they retreive it instead of computing again

## makeCacheMatrix creates a 1x4 matrix containing 4 functions:
## get, set, getinverse and setinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  matrix(c(set, get, setinverse, getinverse), nrows = 1, ncols = 4)
}


## cacheSolve checks if the inverse is stored in cache.
## If not, it computes the inverse and stores it in cache.

cacheSolve <- function(x, ...) {
        m <- x[4,1]()
        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x[2,1]()
        m <- solve(data, ...)
        x[3,1](m)
        m  ## Return a matrix that is the inverse of 'x'
}
