## Put comments here that give an overall description of what your
## functions do

## Matrix inversion may be a costly computation so there may be some benefit to
## caching the inverse of the matrix. This function creates a object that stores the    
## inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
        

## This function will retrieve the stored inversed matrix or compute the inverse of the matrix.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
}
