## Below functions can be used to compute the inverse of a matrix, with caching
## of results for optimization in case of repeated use of the functions on the
## same object.

## This function creates a special "matrix" object, which can be retrieved 
## and manipulated using 4 functions:
##
## set(x): assign x to the matrix object
## get(): return the value of the matrix object
## setinverse(x): assing x to be the cached inverse of the matrix object
## getinverse(): get the cached inverse of the matrix object

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the function will retrieve the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}