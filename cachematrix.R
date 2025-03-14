## The assignment "caching the Inverse of a Matrix" has two functions
## Function 1: makeCacheMatrix 
## Function 2: cacheSolve

## Function 1 returns a list of 4 sub-functions 
## to set and get the 
## matrix values and the inverse of matrix values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Function 2 calculates the inverse of the special matrix x 
## returned by Function 1
## If Function 1 has already calculated the inverse, 
## Function 2 will retrieve inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
