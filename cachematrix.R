## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Creates a cache value containing the matrix
## setIn and getIn write and read from the cached value
makeCacheMatrix <- function(x = matrix()) {
  inX<-NULL
  set <- function(y) {
    x <<- y
    inX <<- NULL}
  
  get <- function() x
  
  setIn<- function(inverse) inX <<-inverse
  getIn <- function() inX
  
  list(set = set, get = get,
       setIn = setIn,
       getIn = getIn)
}


## Write a short comment describing this function
##Calculates inverse matrix unless it exists
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inX <- x$getIn()
  
  if (!is.null(inX)) {
    message("getting cached data")
    return(inX)
  } else {
    inX <- solve(x$get())
    x$setIn(inX)
    return(inX)
  }
}
