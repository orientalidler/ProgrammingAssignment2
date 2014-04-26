## Hi, this is my assignment.
## It contains two parts.
## 1. makeCacheMatrix: create a matrix object that can cache its inverse.
## 2. cacheSolve: compute the inverse of the matrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Function 1. makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) 
        ## Create the list of four small functions
        {
        m <- NULL
        ## The function for setting the Matrix again if neccessary 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## The function of returning the Matrix
        get <- function() x
        
        ## The function for setting the Inverse
        setInverse <- function(inverse) m <<- inverse
        ## The function of returning the Inverse, if it has been cached
        getInverse <- function() m
        ## Return the list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function 2. cacheSolve:

cacheSolve <- function(x, ...) {
        ## Get the matrix from Function 1 above
        m <- x$get()
        if(!is.null(m)) {
                ## Return a noticing message
                message("getting matrix and calculating the inverse")
                ## Calculate the inverse 
                t<-solve(m)
                ## Return the inverse
                return(t)
        }
        ## If m is Null(the Matrix has not cached)
        data <- x$get()
        ## Calculate the inverse
        t <- solve(data, ...)
        ## Cache it
        x$setInverse(t)
        ## Return the inverse
        t
}
