## Put comments here that give an overall description of what your functions do
## The set of functions below help in caching the inverse of a matrix 
## by calculating it only once and storing it in a variable instead of 
## recomputing it each time. 


## Write a short comment describing this function
## makeCacheMatrix returns a vector containing the functions to 
## 1. get the matrix,
## 2. set the matrix, 
## 3. get the inverse of the matrix,
## 4. set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	
	set <- function(y) 		  { x <<-y; inv<<-NULL}
	get <- function()  		  x
	getInverse <- function() 	  inv
	setInverse <- function(inverse) inv <<- inverse

	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve takes as parameter the object created through the makeCacheMatrix
## and returns the inverse of the matrix that is contained in the object. 
## The function solve() is called only once to initialize the inverse and the
## result is cached for a given matrix.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message('getting cached data')
		return(inv)
	}

	data <- x$get()
	inv  <- solve(data, ...)
	x$setInverse(inv)
	inv
}
