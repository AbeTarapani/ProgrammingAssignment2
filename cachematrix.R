## The below functions allow for caching the inverse of a matrix.  Typically
## this is a resource-intensive computation, so caching the inverse calculation
## allows us to avoid running the same calculation repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                                        ##initialize a matrix object 
                                                     ## i as NULL

    set <- function(y) {                             ## initialize your matrix x 
        x <<- y                                      ## for the first time
        i <<- NULL
    }
    get <- function() x                              ## get the matrix stored in x (if any)

    setInverse <- function(inverse) i <<- inverse    ## set your inverse object for 
                                                     ## the first time (hasn't 
                                                     ## previously been cached)

    getInverse <- function() i                       ## get the cached inverse
                                                     ## stored in i (if any)

    list(set = set, get = get,                       ## create a list of functions
         setInverse = setInverse,                    ## with names set, get, setInverse
         getInverse = getInverse)                    ## and getInverse
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()                              ## get current value of i 
                                                     ## in x (if any)
    
    if(!is.null(i)) {                                ## if a value for i exists in
        message("getting cached data")               ## cache then return that value
        return(i)
    }
    data <- x$get()                                  ## get the current value of x

    i <- solve(data, ...)                            ## calculate the inverse of x

    x$setInverse(i)                                  ## store the inverse in cache

    i                                                ## Return a matrix that is 
                                                     ## the inverse of x
}
