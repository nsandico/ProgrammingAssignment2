## This function is similar to the original example

## It also creates a 4-item list with identical set and get functions
##     and setinv and getinv functions

## It gets the matrix argument and stores the set and get functions
## and stores the functions for whether the inverse has been found
## and cached

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Also like the example functions for the mean, this one checks 
## for a cached copy and returns that if possible. If not, it 
## solves for a new inverse and caches it

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
