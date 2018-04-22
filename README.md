## The function makeCacheMatrix creates a special matrix object that can cache its inverse
## See the script below

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  get <- function() m
  setinverse <- function(mean) inverse <<- mean
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(m, ...) {
  inverse <- inverse$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setinverse(inverse)
  inverse
}
  
## Return a matrix that is the inverse of 'x'
x <- matrix(1:6, 2, 3)
m <- makeCacheMatrix(x)
m$get()

cacheSolve(m)
