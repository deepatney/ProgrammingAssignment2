
## The code helps define a new cache matrix and also get its inverse from cache (if it was valculated earlier) or calculate it afresh



## The first function makes a new cache matrix, it initializes m which is the caches value of the inverse and returns a list of four functions that 1. set and get the value of x 2. set and get the value of the inverse of the matrix x


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)



}


## This function checks if the inverse of the matrix was calculated earlier, if so it returns teh stored value. Otherwise it calculates the inverse, returns it and also stores it in cache

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
