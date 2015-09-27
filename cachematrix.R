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
	setinv <- function (inverse) inv <<- inverse
	
## getinv function
	getinv <- function()inv
	
## create list
	list (set = set,get = get, setinv = setinv, getinv = getinv)
}


## The below function cacheSolve calculates the inverse of the matrix created with the 
## function makeCacheMatrix, and it is reusing cached result if it is available.

cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'

## initialize inv using getinv function from above
  inv <- x$getinv()
  
## if inv is not NULL, a message and the cached data is returned  
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
    }

## if inv is NULL, initialize data with the matrix using get function from above
	data <- x$get()
	
## calculate the inverse of the matrix using solve function
	inv <- solve (data,...)
	
## call setinv function from above
	x$setinv(inv)

## return and print inv
	inv
}

