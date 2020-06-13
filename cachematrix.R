## The functions describe in this code create a special "matrix" whose inverse 
## can be cached if it is being used repeatedly without changing to boost the 
## overall speed of the program.


## Matrix inversion is usually a costly computation and therefore it is
## beneficial to cache the inverse of a matrix rather than compute it repeatedly


##Write the following functions:
## The matrix we input should be invertible. That is the assumption in this assignment.
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    doinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         doinv = doinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cacheSolve retrieves the
##inverse from the cache.

##input argument to this matrix is the result of the previous matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinv()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$doinv(i)
    i
}
