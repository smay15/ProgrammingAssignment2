## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse. This is similar to the makeVector funtion that is given as an example above the instruction. However, instead of mean, we are looking for the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) z <<- inverse
        getinverse <- function() z
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getimverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache. This function is similar to the cacheMean function is the example in the instructions except it is not mean, but it is inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
