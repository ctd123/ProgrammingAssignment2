## These functions take a matrix, find its inverse, then cache (store) the results so 
## the inverse can be retrieved from the cache without having to recalculate it.

## function makeCacheMatrix creates a list of 3 functions, set, get, setinverse, and getinverse

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


## Function cacheSolve calls the functions via the list x from the cachematrix function. 

cacheSolve <- function(x, ...) {
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
