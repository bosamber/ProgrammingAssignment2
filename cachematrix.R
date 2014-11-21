## cachematrix.R contains two functions: makeCacheMatrix and cacheSolve
## The first function, makeCacheMatrix, creates an object with 4 functions that
##     - set the value of the matrix
##     - get the value of the matrix
##     - set the inverse of the matrix
##     - get the inverse of the matrix
##
## The second function, cacheSolve, retrieves the inverse of the matrix from 
## this cache when the inverse exists in cache, otherwise it calculates the  
## inverse of the matrix and returns the newly calculated inverse

## Function makeCacheMatrix returns a list of four functions.
## First it sets the cached vale of the input to null
## Then it creates functions to set and get the value of the matrix
## Next it creates functions to set and get the inverse of the matrix
## Last it returns the functions of this object as a list
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                       # initial value of cache is null
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function, cacheSolve,  returns a matrix that is the inverse of 'x'
## First it checks if the inverse was cached (available in memory). If this is
## true, the cache value is returned. 
## If there was no inverse cached, then the original matrix is retrieved from  
## the object, the inverse is calculated, stored in the object and returned as 
## a function result.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
