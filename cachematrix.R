## Calculating and storing the inverse of a matrix in the cache.
## This file consists of two functions, used to calculate the
## inverse of a matrix and store it in the cache. Caching is used
## for time-consuming computations, such as matrix inversions,
## especially when the computation (e.g. inverse) would otherwise
## be evaluated repeatedly.

## The first function, makeCacheMatrix, creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialise 'inv' (which keeps the inverse)
    inv <- NULL
    
    ## The following function stores the matrix and its inverse
    ## in the cache. The inverse is null because the matrix is new
    ## and its inverse has not been calculated yet.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## the following function retrieves the matrix
    get <- function() x
    
    ## the following function stores/updates the cached inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## the following function retrieves the inverse from the cache
    getinverse <- function() inv
    
    ## 'makeCacheMatrix' returns this list, with all the 4 functions
    ## defined above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The following function calculates the inverse of the special
## "matrix" created with the above function (makeCacheMatrix).
## However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the
## matrix (data) and sets the value of the inverse in the cache via
## the 'setinverse' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check if inverse has already been calculated
    inv <- x$getinverse()
    
    ## if the above is not null, get the value from the cache
    ## and return it
    if(!is.null(inv)) {
        ## print an informational message that the result is
        ## retrieved from the cache
        message("getting cached data")
        return(inv)
    }
    
    ## if 'inv' is null, get the matrix
    data <- x$get()
    
    ## and calculate the inverse using 'solve'
    inv <- solve(data, ...)
    
    ## and store the calculated inverse in the cache
    x$setinverse(inv)
    
    ## return the inverse
    return(inv)
}
