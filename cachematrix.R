##Stephen T. Ruzzini
##R Programming
##Assignment 2 - Caching the Inverse of a Matrix

##makeCacheMatrix
##Description:
##	This function takes a cache matrix object, "x", and creates a "cache matrix" object
## from it.  The cache matrix object has the functions: get, set, getInverse, and
## setInverse; which are described in the body of the function.
##Params:
##	x - a matrix object to be converted into a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	##sets the data of the cache matrix object to a new matrix, thus emptying the cache
	set <- function(y)
	{
		x <<- y
		inverse <<- NULL
	}

	##returns the matrix data
	get <- function() x

	##sets the chaced inverse value of the data to "i"
	setInverse <- function(i) inverse <<- i

	##gets the cached inverse value of the data, "invers"
	getInverse <- function() inverse

	##the cache matrix objec that is returned
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


##cacheSolve
##Description:
##	This function takes a cache matrix object, "x", and solves the inverse of it.  If
## the inverse of x has already been solved and is stored in the x's cache, then the
## function will return the value stored in x's cache.  If the invers of x has not been
## solved, then the function will compute x's invers and store it in x's cache
##Params:
##	x - a cache matrix object that will be inversed, or cache read
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        result <- x$getInverse()

        ##Check if the inverse is already in the cache
        if(!is.null(result))
        {
        	message("getting cached data")
        	return(result)
        }

        ##If not, compute it with the "solve" method
        mat <- x$get()
        result <- solve(mat, ...)
        x$setInverse(result)
        result
}
