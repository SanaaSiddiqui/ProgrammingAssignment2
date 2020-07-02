## The 2 functions in combination calculate inverse of a matrix 
## store the result in cache memory

## This makes a Cache Matrix in memory

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m<<- inverse
  getInverse <- function()m
  list(set=set,get= get,setInverse= setInverse,getInverse=getInverse)
}


##This first searches in cache memory if not found then calculates and set in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
