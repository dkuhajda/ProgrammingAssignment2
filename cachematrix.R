## The two functions below work together to calculate and cache (store)
## the inverse of a matrix for faster retrieval than recalculating the inverse
## See example usage at the end of the code.
## David Kuhajda

## The function makeCacheMatrix creates the following functions:
## setmatrix = sets the matrix values
## getmatrix = gets the matrix values
## setinverse = sets the inverse matrix
## getinverse = gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setmatrix <- function(y) {
		x <<- y
		m <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(setmatrix = setmatrix, getmatrix = getmatrix,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The function cacheSolve returns the inverse of the matrix
## If the inverse had previously been calculated, the inverse
## is cache (stored) for retrieval instead of recalculating the 
## inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	data <- x$getmatrix()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

## Example usage of the functions.

## This loads the cachematrix.R code:
## 	source("cachematrix.R") 
##
## This initializes the functions created by makeCacheMatrix and sets
## the initial matrix values in matrix a:
## 	a <- makeCacheMatrix(matrix(c(1,3,7,8), nrow=2,ncol=2)) 
##
## The cacheSolve will return the inverse of the matrix if one exists
## by either returning the previously calculated inverse if one exists
## or calculating the new inverse:
## 	cacheSolve(a)
##
## This will change/set the value of the matrix in a:
## 	a$setmatrix(matrix(c(2,3,5,7,11,13,14,1,19), nrow=3, ncol=3))
##
## This will get/return the value of the matrix stored in a:
## 	a$getmatrix()
##
## This will get/return the value of the inverse for a that is cached:
## 	a$getinverse()
##
## This will set the matrix values that are stored as the inverse:
## 	a$setinverse()
## Note: this is not really useful outside of the cacheSolve function.

