
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function take a matrix and save it and prepare it to
## calculate the inverse, save it in cache and use it every time
## without recalculate it, with big matrices take a long time.
## I add a easy way to enter the matrix in set function and make 
## the standard way using all sintaxis of R.
## Always the matrix is changed, the inverse is NULL, and it must be 
## recalculated.

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

## this function calculate the inverse of matrix using the R function Solve.
## the difference in this function it must be calculated only when de matrix
## is changed, 

cacheSolve <- function(x, ...) {
  ## get the value of the inverse of the matrix
  inverse <- x$getInverse()
  ## if the value is not null, the function return the value from getInverse
  if (!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  ## if not the inverse must be recalculated
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
  
  ## Return a matrix that is the inverse of 'x'
}