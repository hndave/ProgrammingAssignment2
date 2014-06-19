#Author : Hitesh Dave
# R Programming Assignment

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
	# Defaul constructor for this object sets the invMatrix to NULL, 
	#Usage: a <- makeCacheMatrix(rep(1,6),2,3)
	#x is set in the local env via input parameter
	invMatrix <- NULL
	
	
	# setter method, sets the input 'y' to the matrix and invMatrix to NULL
	#usage: m <- makeCacheMatrix$set(matrix(1:6, 2, 3))
	set <- function (y) {
		x <<- y
		invMatrix <<- NULL	
	}

	# Getter for x
	get <- function () x

	#Setter for invMatrix
	setInverse <- function (inverseMatrix) {
		invMatrix <<- inverseMatrix
	}

	#Getter for invMatrix
	getInverse <- function () invMatrix

	#Return the special list 'matrix' object
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

#cacheSolve takes a special 'matrix' object X as an input parameter 
#and sets the inverse of the matrix stored in X
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		#Check if inverse already exists?
		if(!is.null( x$getInverse() )) {
			# Do Nothing
			message ("Inverse Already exits !")
		} else {
			#call the setInverse method and pass the result of the solve(x$get(), ...)
			x$setInverse( solve(x$get(), ... ))
		}
		x$getInverse()
}
