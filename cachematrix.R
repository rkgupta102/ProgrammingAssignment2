## There are two functions makeCacheMatrix and cachesolve. 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## makecacheMatrix is doing the operation as explained above. This generates a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(p) {
    x <<- p
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matr <- x$get()
  i <- solve(matr, ...)
  x$setinverse(i)
  i
}