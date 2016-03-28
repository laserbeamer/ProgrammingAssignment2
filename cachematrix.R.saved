## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function to cache matrice inverse with a get and set and imverse methods
makeCacheMatrix <- function(x = matrix()) {
 
        #Define and Initialize the Cache Matrix
	mCache <- NULL
	
	set <- function(y) {
	    x <<- y
	    mCache <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(inv) mCache <<- inv
	
	getinverse <- function() mCache
	
	list(set = set, 
	     get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse
	    )

}


## Write a short comment describing this function
## Check if Matrix has already been iverted and returned it if so,
## otherwise invert, cached and returned inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mCache <- x$getinverse()
	
	if (!is.null(mCache)) {
            message("returning cached matrix data")
	    return(mCache)
	}
        
	xData <- x$get()
	
	mCache <- solve(xData, ...)

        x$setinverse(mCache)

        mCache
}
