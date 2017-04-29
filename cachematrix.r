## These pair of functions will cache the inverse of a matrix

## Function 1 named makeCacheMatrix is a function that creates
## a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function 2 computes the inverse of the special "matrix" returned
## by function 1 (makeCacheMatrix) above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache and send a message


cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retrieving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Testing the function

testmatrix <- matrix(c(1:4),2,2)
cachematrix <- makeCacheMatrix(testmatrix)
cacheSolve(cachematrix)##inverse returned from computation (no message)
cacheSolve(cachematrix)##inverse returned from cache (message)

