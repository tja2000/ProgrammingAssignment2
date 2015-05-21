## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
	# No inverse yet, set to NULL
	InverseMatrix <- NULL
	
	# Get/Set for matrix
	Set <- function(y) { 
						x <<- y 
						InverseMatrix <<- NULL	
						}
	Get <- function() x
	
	# Get/Set for the Inverse matrix
	SetInverse <- function(t) InverseMatrix <<- t
	GetInverse <- function() InverseMatrix
	
	list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		# Is it in the cache?
		m <- x$GetInverse()
		if (is.null(m)) {
			## Nope. Get and inverse.
			m <- solve(x$Get())
			## Store in cache list.
			x$SetInverse(m)
			message("Cache miss, stored in cache")
				}
		else {
			# In cache...
			message("Cache hit, returning inverse")
		}	
		return (m)
}

