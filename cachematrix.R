## this function creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inverse <- function(inv) i <<- inv
        get_inverse <- function() i
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## This function calculates the inverse of the special "matrix" created in makeCacheMatrix
## First it checks to see if the inverse has already been calculated, if it has, it gets the mean from
## the cache and skips the computation, otherwise it calculates the inverse and sets the value of the 
## inverse in the cache via the set_inverse function

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}