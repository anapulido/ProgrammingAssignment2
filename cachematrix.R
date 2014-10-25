## This pair of functions cache the inverse of a matrix, so that you don´t have
## to compute it repeatedly

## This function creates  a precial "matrix" object, which is really a list
## containing a function to set the value of a matrix, get the value of the matrix
## set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmx <- function(solve) m <<- solve
        getmx <- function() m
        list(set = set, get = get,
             setmx = setmx,
             getmx = getmx)
}


## This function calucaltes the inverse of the special object "matrix" created 
## with makeCacheMatrix, but if first checks if the inverse has already been
## calculated, so if it was calculated previously, it gets the inverse from
## the cache and skips the computation, otherwise, it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmx(m)
        m
}
