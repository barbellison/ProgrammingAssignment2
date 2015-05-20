## These functions will store an inverse matrix in cache for future use.
## The makeCacheMatrix function creates a special matrix that is really a list that contains a function
##which sets the value of the matrix, gets the value of the matrix, 
##sets the value of the inverse matrix, and gets the value of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function(y) { 
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setreverse <- function(reverse) r <<- reverse
        getreverse <- function() r
        list(set = set, get = get, setreverse = setreverse, getreverse = getreverse)
}


## The cacheSolve function will look to see if the inverse matrix exists and
##return it.  If the inverse matrix does not exist, it will calculate it and store it in cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getreverse()
        if (!is.null(r)) {
                message("get cached data")
                return(r)
        } 
        data <- x$get()
        r <- solve(data, ...)
        x$setreverse(r)
        r
}
