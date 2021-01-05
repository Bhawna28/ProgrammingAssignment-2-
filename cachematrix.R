## Functions developed here are meant to save the computation time significantly.The first 
## function is supposed to compute, allocate and store the inverse of a given matrix. Whenever 
## the second function is used to calculate the inverse of a matrix, it will first check if the inverse 
## is stored in cache already by calling the first function. If the inverse is stored,then it will return the stored inverse
## ,otherwise th einverse will be computed.

## This function computes,allocates and stores the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
a<-makeCacheMatrix(x=matrix(c(2,3,5,9),nrow = 2,ncol=2))

## This function uses the previous function to cache the stored inverse, or computes the inverse if not available already.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
## Return a matrix that is the inverse of 'x'
cacheSolve(a)
