## There are two functions below.  
## The first one creates a cache of a matrix. 
## The second function creates the inverse of a matrix.


## This function creates a cache for the inverse of a matrix

makeCacheMatrix <- function(mtx = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the matrix
## returned by `makeCacheMatrix. If the inverse has
## already been calculated and is the same, then
## cacheSolve willget the inverse matrix already in the cache.

cacheSolve <- function(mtx, ...) {
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
                message("Getting cached data...")
                return(inverse)
        }
        data <- mtx$get()
        invserse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)