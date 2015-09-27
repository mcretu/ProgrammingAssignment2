## This is an assignmemt containing two functions in order to 
## cache the inverse of a matrix.

## For a verification example:
##> m<-makeCacheMatrix(matrix(1:4,2,2))
##> m$get()
##     [,1] [,2]
## [1,]  1   3
## [2,]  2   4
## > cacheSolve(m)
##     [,1] [,2]
## [1,]  -2   1.5
## [2,]  1   -0.5


## The first function makeCacheMatrix creates a special matrix 
##(square invertible matrix) which is a list containing these functions:
##  1) set the value of the matrix: set
##  2) get the value of the matrix: get
##  3) set the value of the inverse matrix: setinv
##  4) get the value of the inverse matrix: getinv

makeCacheMatrix <- function(x = matrix()) {
## initialize inv to NULL
  	inv <- NULL
## set function
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
## get function
	get <- function()x
## setinv function
	setinv <- function (inverse) inc <<- inverse
## getinv function
	getinv <- function()inv
	
	list (set = set,get = get, setinv = setinv, getinv = getinv)
}


## This function calculate the inverse of the matrix created with the 
## function makeCacheMatrix, and it is reusing cached result if it is available.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
  }
  
	data <- x$get()
	inv <- solve (data,...)
	x$setinv(inv)
	inv
}

