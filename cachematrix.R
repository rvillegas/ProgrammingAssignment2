## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	iMx<-NULL
	## iMx is inverse matrix
	## set is used to simplify chrging the data, 
      ## do not need matrix, 2 dimensions, etc
	set  <- function(dimension,y) {
	x <<-matrix(y,dimension,dimension)
	iMx <-NULL
	}
	## setM is standard way to enter the matrix
      setM <- function(y) {
                x <<- y
                iMx <<- NULL
       }
	get <-function() x
	## I do not use set inverse because it must be calculated
	setInverse <- function(inverse) iMx<<-inverse
	getInverse <- function() cacheSolve(x)
	list(set=set, get=get, setM=setM,
	setInverse=setInverse,
	getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	  inverse <- getInverse()
	  if (!is.null(inverse)){
		message("getting cached data")
		return (inverse)
		}
        data <- x$get()
	  inverse <- solve(data)
	  x$iMx <-inverse
	  inverse


        ## Return a matrix that is the inverse of 'x'
}
