# makeCacheMatrix for a new function. It contains a matrix
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {m}
  setInverse <- function(inverse) {i <<- inverse}
  getInverse <- function() {i}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve function is used to check our inverce function of our matrix 
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat) %*% mat
  x$setInverse(m)
  m
}


