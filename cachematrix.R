## Inverse Matrix Caching
## The following will cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {

	# Set a default value for the matrix
	m <- NULL
	
	# Change the stored matrix, and reset the cache
	set <- function(y) {
		# Set the new matrix value
		x <<- y
		# Reset the cached value (scoped at function level)
		# Set a default value for the matrix
		m <<- NULL
	}
	
	# A function to retrieve the original matrix
	get <- function() x
	
	# Store the inverse of the matrix
	setInverse <- function(Matrix) m <<- Matrix
	
	# Retrieve the stored inverse of the matrix
	getInverse <- function() m
	
	# return the functions as a list so they are available outside the function
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## Solve a cashed matrix, and store the inverse matrix
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	# get inverse of the matrix
	m <- x$getInverse()
	
	# verify whether the returned value is null
	if(!is.null(m)) {	
		# if the inverse exists and is not null, return the cashed version
		return(m)
	}
	# continue of no cached version exists
	
	# get the matrix stored in x
	data <- x$get()
	
	# use the "solve" function to calculate the inverse of the matrix 
	m <- solve(data, ...)
	
	# store the inverse for future lookups
	x$setInverse(m)
	
	# return the solved inverse of the matrix
	m
}


## Validation

c1<-rbind(c(1, -1/4, 2), c(-1/4, 1, 2), c(-5, 4, 1))  
c1
c2<-makeCacheMatrix(c1)
c3<-cacheSolve(c2)
c3
c4<-makeCacheMatrix(c3)
c5<-cacheSolve(c4)
c5



