## R Programming Assignment 2 in this assignment we create 2 functions to manage the calculation of inverse matrices


## The first function creates a special list that contains functions that allow for the assignment of a matrix,
## the calculation of the inverse, and the retieval of those two items.


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(i) inv <<- i
     getinv <- function() inv
     list(set=set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## The second function is called to retrieve the inverse of the matrix,  It ensures efficiency by first checking that
## if the inverse has already been calculated it returns that value.  If it has not then it does the calculation and 
## saves it for the future and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
