## makeCacheMatrix creates the matrix object

makeCacheMatrix <- function(x = matrix()) {
     # Setting the value of the matrix
     Invert <- NULL
     set <- function(y) {
          x <<- y
          Invert <<- NULL
     }
     # Getting the value of the matrix
     get <- function() x
     # Setting the value of the inverse of the matrix
     setInvert <- function(inverse) Invert <<- inverse
     # Getting the value of the inverse of the matrix
     getInvert <- function() Invert
     list(set = set, get = get,
          setInvert = setInvert,
          getInvert = getInvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Computes the inverse of matrix created in makeCacheMatrix
        ## Check for existence of matrix
     Invert <- x$getInvert()
     if(!is.null(Invert)) {
          message("getting cached data")
          return(Invert)
     }
          ## Compute the inverse
     data <- x$get()
     Invert <- solve(data)
     x$setInvert(Invert)
          ## Print the output
     Invert
}
