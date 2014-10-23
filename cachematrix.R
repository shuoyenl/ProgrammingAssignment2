## Matrix inversion is usually a costly computation and their may be some 
#  benefit to caching the inverse of a matrix rather than compute it repeatedly. 
#  Below are two functions that are used to create a special object that stores
#  a square matrix and cache's its inverse.
##

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special matrix returned by 
#  makeCacheMatrix above. If the inverse has already been calculated (and the
#  matrix has not changed), then the cachesolve should retrieve the inverse 
#  from the cache.
##
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i  
}