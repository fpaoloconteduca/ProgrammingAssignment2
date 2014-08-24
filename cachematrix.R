## This function caches the value of the inverse of an invertible matrix


## This function creates a list that sets the value of the matrix, get the 
## the value of the matrix, set the value of the inverse matrix, get the 
## value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	  set <- function(y) {
		x <<- y 
		m <<- NULL
	  }
	  get <- function() x
	  setinverse <- function(solve) m <<- solve
	  getinverse <- function() m
	  list(set=set, get=get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function calculates the inverse of the list created by the above 
## function. It checks whether the inverse matrix has already been calculated
## in order to save computation time. Then it calculates iff the inverse matrix
## has not been calculated yet via the set inverse function using "solve".

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
	  if(!is.null(m)) {
		message("getting cached data")
	  return(m)
	  }
	  matrix <- x$get()	
	  m <- solve(matrix, ...)
	  x$setinverse(m)
	  m
}
