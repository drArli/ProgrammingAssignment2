## The function makeCacheMatrix creates a "special" matrix object,
## caches its inverse and returns a list of functions that act 
## on this "special" matrix object:

## set - sets the matrix
## get - return the matrix 
## set_inv - sets the inverse of the matrix
## get_inv - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	mat_inv <- NULL # variable to hold the inverse of the matrix
	set <- function(y){
		
		x <<- y
		mat_inv <<- NULL

	}

	get <- function() x
	set_inv <- function(inv) mat_inv <<- inv
	get_inv <- function() mat_inv
	
	list( set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}




## The cacheSolve function calculates the inverse of the "special"
## matrix object. If first checks if the inverse has already been
## computed and stored, otherwise the function compute the matrix
## inverse, using the function solve, stores the inverse in cache 
## and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	mat_inv <-x$get_inv()
	
	if( !is.null(mat_inv) ){
	
		message("getting cached data!")
		return(mat_inv)	
	}

	data <- x$get()
	
	mat_inv <- solve(data)
	x$set_inv(mat_inv)
	mat_inv

}
