## This function generates the inverse of a matrix, but does so by caching previous results and retrieivng
## them if the call is a duplicate. This follows the structure of the vector mean example given in the write-up
## to this assignment


## This function creates a "matrix" which is really a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get=get,
             setinv = setinv,
             getinv = getinv)

}


## This function ...
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
