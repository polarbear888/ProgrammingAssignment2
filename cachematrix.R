## The assignment is to write a pair of functions that cache
## the inverse of a matrix for efficient computation

## preps a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}

## computes the inverse of matrix from makeCacheMatrix function
cacheSolve <- function(x, ...) {
  
  b <- x$getinverse()
  
  if(!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  
  dat <- x$get()
  b <- solve(dat, ...)
  x$setinverse(b)
  b
}
