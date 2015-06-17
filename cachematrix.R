# Assignment: Caching the Inverse of a Matrix
# WEEK 3 PROGRAMMING QUIZ
# SCOTT WESTENBERGER
# JUNE 09, 2015

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 
# These two functions cache the inverse of a matrix in order to avoid multiple computations.

# makeCacheMatrix: 	This function creates a special "matrix" object 
#                   that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inversion <- NULL
	set <- function(y) {
		  x <<- y
		  inversion <<- NULL
	}
	get <- function() x
	setInversion <- function(invertedMatrix) inversion <<- invertedMatrix
	getInversion <- function() inversion
	list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}


# cacheSolve: 	This function computes the inverse of the special "matrix" 
# 				returned by makeCacheMatrix above. If the inverse has already been 
# 				calculated (and the matrix has not changed), then the cachesolve 
# 				should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inversion <- x$getInversion()
	if(!is.null(inversion)){            # if the inverted matrix HAS been cached, retrieve it
	  message("getting cached data")
	  return(inversion)
	} 									# if the inverted matrix has NOT been cached...

	dataMatrix = x$get()                # retrieve original matrix
	inversion = solve(dataMatrix)       # perform inversion
	x$setInversion(inversion)           # cache result
	inversion                           # return result
}
  











