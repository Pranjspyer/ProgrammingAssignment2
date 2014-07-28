##This file consists of functions which are used calculate the inverse of a given matrix
## and the result will be stored in a cache. If we need to retrieve the maxtrix inversion
##result multiple times then instead of calculating the value of inverse again and again
## these methods will return the result stored in the cache.


## This method creates a cache matrix and returns a closure (list) which has
## the data associated with the cache matrix.

makeCacheMatrix <- function(x = matrix()) 
  {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  # Getter for the inverse
  getinv <- function() inv
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## This function will check the result of matrix inversion in the cache. If the
## result is available then it is returned. If the result is not available then
## the matrix inversion is calculated and the result is stored in the cache.
cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}
