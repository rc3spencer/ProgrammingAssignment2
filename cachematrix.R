# these functions are used to cache the inverse of a matrix.
# makeCacheMatrix creates a list containing a function to
# Do : set the value, get the value of the matrix, set and get the val of inverse of matrix.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        	x <<- y
			inv <<- NULL
}

        get <- function() x
		setinverse <- function(inverse) inv <<- inverse
		getinverse <- function() inv
		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}





# Returns that inverse of the matrix. check if
# the inverse is there. If . . get the result and skips the
# computation. If not, it redo it, set the value in the cache via
# function (setinverse).
# Assume the matrix is always invertible.

cacheSolve <- function(x, ...) {
			inv <- x$getinverse()

			if(!is.null(inv)) {
					message("getting cached data.")
					return(inv)
}

			data <- x$get()
			inv <- solve(data)
			x$setinverse(inv)
			inv
}



