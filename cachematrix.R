## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. The following two functions are used to cache the
## inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse value of the matrix
## 4. get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    ## Assign a e value to an object in an environment
    ## difference from the current environment.
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result 
## and skips the computation. If not, it computes the inverse, sets 
## the value in the cache via setInv function.

## This function assumes that the matrix can be inverted.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  # Check if the inverse was calculated, else, calculate the inverse
  if(!is.null(inv)) {
    message("getting cached data...")
  } else {
    # The following will calculate the inverse
    data <- x$get()
    inv <- solve(data)
    
    # Set the inverse valaue of the matrix
    x$setInv(inv)
  }
  
  # Return the inverse of the matrix
  inv
}
