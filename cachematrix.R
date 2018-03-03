## This function creates a special "matrix" object that can cache its inverse.
## It contains list of functions to set and get a matrix,and then
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
		inv = NULL
		set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setInverse = function(inverse) inv <<- inverse 
        getInverse = function() inv
        list(set=set, 
			get=get, 
			setInverse=setInverse, 
			getInverse=getInverse)
}




## The function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated, then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
	inv <- x$getInverse()
        
	# if inverse has already been calculated
	# don't compute ... instead get it from the cache
		
	if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
       # else compute the inverse
		mat <- x$get()
        inv <- solve(mat, ...)
        
		#setting inverse value in the catch 
		x$setInverse(inv)
        return(inv)
}
