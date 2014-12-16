## R functions for calculating and caching the inverse of a matrix
## Coursera R Programming Course   Dec 2014

## Create a vector that includes the get/set values of the vector and the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	#getinv and setinv are to get and set the inverse of a matrix
        setinv <- function(inv) m <<- inv 
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## Calculate the inverse of a matrix using makeCacheMatrix, but first check to see if it is in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
