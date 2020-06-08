## This functions are written for Coursera R programming course 
## week 3 assignment 2

## makeCacheMatrix
## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mInv<- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mInv <<- inverse
  getInverse <- function() mInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
  mInv <- x$getInverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data, ...)
  x$setInverse(mInv)
  mInv
        ## Return a matrix that is the inverse of 'x'
}
