## Put comments here that give an overall description of what your
## functions do

## creates an object storing the cached inverted matrix and
## the solve function to use if the inverted matrix is requested
## but not yet computed
## like the example for the vector this this function contains a list
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverted matrix
## 4.  get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	# the inverted matrix, NULL means not yet set
	m <- NULL

	# 1. set the original matrix
	set <- function(y) {
		x <<- y
                m <<- NULL
       	}

	# 2. get the original matrix
        get <- function() x

	# 3. set the inverted matrix
        setsolve <- function(solve) m <<- solve

	# 4. return the inverted matrix, NULL if not yet computed
        getsolve <- function() m

	# return the object
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## return the inverted matrix for x
## x must be a matrix object created with makeCacheMatrix

cacheSolve <- function(x, ...) {
	# check if already solved
	m <- x$getsolve()
        if(!is.null(m)) {
        	message("getting cached data")
		return(m)
        }

	# get original matrix
       	data <- x$get()
	# call solve to invert the matrix
       	m <- solve(data, ...)
	# cache inverted matrix
       	x$setsolve(m)
        # Return the inverted matrix
        m
}

# how to use (adapted from example in help(solve):
#     hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#     h8 <- hilbert(8)
#     h8o <- makeCacheMatrix(h8)
#     cacheSolve(h8o)