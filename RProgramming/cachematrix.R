## These functions are intended to show the way R's lexical scoping can be
## used to create objects that contain a set of "state" data which is
## accessible in multiple environments.
##
## Here, some data (a matrix) is stored along with some information about that
## data (the matrix inverse) that might be time- or memory-consuming to
## compute.
##
## Note that the assignment operator <<- allows the values of the variables
## within the makeCacheMatrix function environment to be changed by the
## makeCacheMatrix child functions, creating a mutable state.

# This function accepts a matrix and returns an object with methods to
# set and get both the matrix itself and its inverse 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # Overwrite the matrix x, set inverse to unknown
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    # Set/get the inverse. Note that the inverse is set to whatever
    # value is provided to the setinv function and thus is not
    # necessarily the inverse of x.
    setinv <- function(val) inverse <<- val
    getinv <- function() inverse
    
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

# This function accepts a matrix created by makeCacheMatrix and returns
# the result of the solve() function (the matrix inverse), which may 
# already be stored in the cacheMatrix object
cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    
    if( !is.null(inverse) ) {
        message("Returning cached inverse...") 
    }
    else {   
        message("Computing inverse with solve()...")
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
    }
    inverse
}
