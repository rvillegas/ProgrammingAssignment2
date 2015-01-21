
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## iMx is inverse matrix  
  iMx<-NULL
  ## set -> Easy way to enter a matrix
  set  <- function(dimension,y) {
    x <<-matrix(y,dimension,dimension)
    iMx<<-NULL
  }
  ## setM -> Standard way to enter a matrix, trying to follow the vector example
  setM <- function(y) {
    x <<- y
    iMx<<-NULL
  }  
  get <- function() x
  getInverse <- function() iMx
  setInverse <- function(inverse) iMx <<- inverse

  list(set=set, get=get, 
       setInverse=setInverse,
       getInverse=getInverse,
       setM=setM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
  
  ## Return a matrix that is the inverse of 'x'
}