## Course: "R Programming For Data Science" @ John Hopkins
## Assignment 2: Caching the Inverse of a Matrix
## By: https://github.com/wballaa/

## Note: For testing code, please refer to test procedure presented 
## for the example code in the assignment @ https://www.coursera.org/learn/r-programming/module/6BaZ2/discussions/eauvgAi7EeaC2Qp7KBUB1w 

## makeCacheMatrix: Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
	   m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: Finds invesrese of a matrix. Uses cached value if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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