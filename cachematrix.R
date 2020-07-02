## The 2 functions in combination calculate inverse of a matrix 
## store the result in cache memory

## This makes a Cache Matrix in memory

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){ ## Set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function()x  ## Get the value of the matrix
  setInverse <- function(inverse) m<<- inverse ## Set the inverse of the matrix
  getInverse <- function()m  ## Get the inverse of the matrix
  list(set=set,get= get,setInverse= setInverse,getInverse=getInverse)
}


##This first searches in cache memory if not found then calculates and set in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()  ##get the inverse as set in the makeCacheMatrix(if there )
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  ##return the cache computed inverse matrix
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
