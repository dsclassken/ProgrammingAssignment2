## Create "cached matrix" for use by 'cacheSolve' function.
## if matrix is unchanged, use cached value instead of
## using 'solve' function.    

## Use this first to create a "cached matrix' object.

makeCacheMatrix <- function(x = matrix()) {
    m  <- NULL
    set  <- function(y) {
        x <<- y
        m <<- NULL
    }
    get  <- function() x
    setinv  <- function(solve) m <<- solve
    getinv  <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## use this function to get the inverse of the "cached matrix' object.

## sample test code. 
##    a <- c(1,2)
##    b <- c(3,4)
##    x <- rbind(a,b)
##    x2 <- MakeCacheMatrix(x) 
##    x2inv = cacheSolve(x2)

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
}
