## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
              x <<- y
              inverse <<- NULL
      }
        get <- function() x
        set_inverse <- function(solve) inverse <<- solve
        get_inverse <- function() inverse
        list(set = set, get = get,
             setinv = set_inverse,
             getinv = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
      }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}

