##makeCacheMatrix and cacheSolve are functions that compute the inverse of a square matrix and cache in it memory so that it can be called, 
##rather than recomputed, which can be time-consuming for large matrices. The input of cacheSolve is the object where makeCacheMatrix is stored.   

## makeCacheMatrix is a function, whose purpose is to create a "matrix", x, that can cache its own inverse. It accomplishes that by storing
## 4 functions: 'set()' sets the matrix, 'get()' gets the matrix, 'setinv()' sets the inverse, and 'getinv()' gets the inverse.


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) { 				# 'set' substites the inverse stored in the main function by
		x <<- y 					# assigning the value of y to x 
		inv <<- NULL 				# It restores inv to NULL because the value of matrix has been reset, 
								# and hence the old inverse has to be nullified.
	}
	get <- function() {				# Returns the value of the matrix stored in the function
		x
	}	
	setinv <- function(inverse) {			# Stores the value of the input, inverse, to inv
		inv <<- inverse
	}
	getinv <- function() {				# Returns value of inv
		inv 
	}
	
	list (set = set, get = get, 			# Returns object of the function 'makeCacheMatrix' is a list, which
	setinv = setinv, getinv = getinv)  		# stores the 4 functions defined in 'makeCachematrix'.

}


## cacheSolve outputs the inverse of the "matrix", created with makeCacheMatrix.
## First, it checks whether the inverse exists in memory and is not null. If so, it gets the inverse
## from the cache and skips computation. Otherwise, it calculates the inverse of the matrix stored in makeCacheMatrix, 
## and sets the value of the inverse to the cache for future calling.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()						
	if (!is.null(inv)) {				
 		message("getting cached data")	 
		return(inv)					
	}
	data <- x$get()					
	inv <- solve(data, ...)				 
	x$setinv(inv)					
	return(inv)						
}
