##------------------------------------------------------------------------------
## Functions to cache time consuming computations.  These functions are used to  
## calculate the inverse of a matrix and cached the result for repeated 
## calculation requests.  These functions make use facilities such as lexical 
## scoping, solve function and cached syntax for R to acommplished the 
## functionalities.
##
## makeCacheMatrix function create a list that holds accessors and mutators of 
## the "matrix".
##------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##------------------------------------------------------------------------------
## cacheSolve function used to calculate the inverse of a matrix.  This function
## uses makeCacheMatrix to check for existing cached calculation.  If cached 
## matrix exists, then return the cached value, otherwise, do the calculation and 
## cache the result prior to returning the result.
##------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
