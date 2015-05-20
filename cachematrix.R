## First function creates an object that can store the inverse of a matrix
## Second function returns the cached inverse, if available; if not, it calculates and stores it first

## Create an object that can store the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invx <<- solve
  getinverse <- function() invx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the cached inverse, if available; if not, it calculates and stores it first

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinverse(invx)
  invx
}
