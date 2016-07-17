## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        # A function to change the matrix saved as x
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        # A get function to return the matrix x
        get <- function() x
        
        # A function to set the value of s to the value of solve(x)
        setsolve <- function(solve) s <<- solve
        
        # A function to return s
        getsolve <- function() s
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        reverse <- x$getsolve()
        # Checking if reverse of x is already calculated
        if(!is.null(reverse)){
                message("Getting cached data")
                return(reverse)
        }
        
        data <- x$get()
        reverse <- solve(data)
        x$setsolve(reverse)
        reverse
        
}

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
