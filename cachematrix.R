## R Programming
## Week 3
## Programming Assignment 2
## March 20, 2016
## By Randy Evered

## These functions allow caching the inverse of a matrix,
## so that the inverse does not have to be repeatedly solved.


## makeCacheMatrix - sets up the functions needed to cache
## the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve -

cacheSolve <- function(x, ...) {

	## See if it's already been caculated
        i <- x$getinverse()

        if (!is.null(i)) {
                message("Getting cached matrix inverse data")
                return(i)
        }

	## Calculate the matrix inverse once
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)

	message("Inverse calculated and stored")

        ## Return a matrix that is the inverse of 'x'
        i
}
