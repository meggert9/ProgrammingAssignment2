## These functions create a special matrix object and allow the inverse of that matrix 
## to be cached.

## This function creates a "special" matrix, which is actually a list of four functions.
## These four functions set and get the values of an input matrix, as well as set and get
## the values of the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" that was created with 
## the makeCacheMatrix function. If the inverse has already been
## calculated, the function retrieves it from the cache. Otherwise, it calculates 
## the inverse of the matrix and puts that matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
