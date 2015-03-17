## With these functions we create a special matrix then either bring its previously calculated inverse stored in the Cache 
## or calculates it if it has not been done before

## Creates the matrix

makeCacheMatrix <- function(x = matrix()){
	r <- NULL
	set <- function(y) {
		x <<- y
		r <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) r <<- solve
	getsolve <- function() r
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
}


## Looks up the matrixs inverse or calculates it if not found

cacheSolve <- function (x, ...){
	r <- x$getsolve()
	if(!is.null(r)) {
		message("getting cached data")
		return(r)
	}
	data <- x$get()
	r <- solve(data, ...)
	x$setsolve(r)
	r
}
