## Functions below do the following: 
## makeCacheMatrix creates a "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix, 
## or if the inverse has already been calculated and matrix has not changed, 
## then the cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # the invrerse matrix is made NULL
  set <- function(y) {
    x <<- y
    m <<- NULL  # the invrerse matrix is made NULL because the initial matrix has been changed
  }
  get <- function()    x
  setinverse <- function(inv)    m <<- inv
  getinverse <- function()    m
  
  list(s = set, g = get, si = setinverse, gi = getinverse)
}

## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix 
## or retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$gi()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$g()
  m <- solve(data, ...)
  x$si(m)
  m
}