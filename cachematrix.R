## this function will calculate the inverse of a matrix and store it ##in the cache to use it later

## in the make cache matrix function you will define the matrix and make the setup to call it from the cache

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


## cache solve will calculate the inverse of the matrix establish in 
## makecache matrix function if is not already calculated if thats
##the case the function will get the inverse from cache and display ## it

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
