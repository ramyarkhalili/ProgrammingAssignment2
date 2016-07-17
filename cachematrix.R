## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix function will get a matrix and assigns its inverse to another
# cached matrix so there will be no need to recalculate it.
# 
# There are 2 variables in this function:
# 1. x is a matrix we want to reverse
# 2. s is the reverse of the matrix.
#
# There are 4 functions whitin this function:
# "set" which sets the matrix and also sets its initial reverse (before
#       computation) as NULL
# "get" which returns the matrix
# "setsolve" which sets the inverse of the matrix to s
# "getsolve" which returns the value stored in s

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
# cacheSolve function will compute the reverse of a matrix which is created by
# makeCacheMatrix function. Then, it checks whether the inverse is already 
# computed or not. If it was computed, it returns the value associated with it.
# If not, using solve() function, it computes the reverse and assigns it to the
# matrix's correspondig variable (s)
# 
# It has 2 arguments:
# "x" is the matrix, an object of makeChacheMatrix
# "reverse" is the reverse of x
#
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
