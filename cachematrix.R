#Author : Hitesh Dave
# R Programming Assignment
# Function to create a special 'matrix' object. 
# -  x is set to the matrix object created
# -  invMatrix - stores the inverse of the matrix X.
# -  Function defines four methods for getters/setters for the two local variables X, invMatrix
# -  a list of these four methods is returned to the caller.

#Usage: a <- makeCacheMatrix(rep(1,6),2,3)
# 	a$set(matrix(1:6, 2, 3)) , set a matrix in a
#	m <- a$get(), returns the stored matrix
#	im <- a$getInverse(), returns the stored inverse of the matrix
#	a$setInverse(matrix), sets the matrix to the  inverse matrix variable

makeCacheMatrix <- function(x = matrix()) {
	# Defaul constructor for this object sets the invMatrix to NULL, 
	#x is set in the local env via input parameter
	invMatrix <- NULL
	
	
	# setter method, sets the input 'y' to the matrix and invMatrix to NULL
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

		#Check if inverse already exists?
		# if Yes, do nothing, just print a message that returning cached value`
		if(!is.null( x$getInverse() )) {
			# Do Nothing
			message ("Inverse Already exits!, cached value returned")
		} else {
			#invoke the setInverse method on 'x' and pass the result of the solve(x$get(), ...)
			x$setInverse( solve(x$get(), ... ))
		}

		## Return a matrix that is the inverse of 'x'
		x$getInverse()
}
