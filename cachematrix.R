## Caching the Inverse of a Matrix

## makeCacheMatrix function stores a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set_matrix <- function(y) {
              x <<- y
              inverse <<- NULL
      }
        get_matrix <- function() x
        set_inverse <- function(solve) inverse <<- solve
        get_inverse <- function() inverse
        list(setmat = set_matrix, 
             getmat = get_matrix,
             setinv = set_inverse,
             getinv = get_inverse)
}


## cacheSolve function takes an argument of a "matrix" object and returns the inverse.
## It will check whether the inverse has been calculated in the makeCacheMatrix function or not.
## If yes, then a message will appear and the cached inverse in makeCacheMatrix function will be returned.
## If not, it will skip the 'if' statement and calculate the inverse with 'solve' function in its own body ...
## ... then, cache the inverse using 'set_inverse' method of makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)){
            message("getting cached and inversed matrix")
            return(inverse)
      }
        matrix <- x$getmat()
        inverse <- solve(matrix, ...)
        x$setinv(inverse)
        inverse
}

