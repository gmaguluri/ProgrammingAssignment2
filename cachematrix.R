# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list of functions to
# 1. set the value of the matrix and reinitialize inv so cache is empty
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
   
 makeCacheMatrix <- function(x=mmatrix()) {
         
     # holds the cached value or NULL if nothing is cached
     # Initially nothing is cached so set it to NULL
         
         inv <- NULL
         
         # store a matrix
         set <- function(y) {
         x <<- y
         # since the matrix is assigned a new value, flush the cache
         inv <<- NULL
     }
         
     # returns the stored matrix
     get <- function() x
         
     # cache the given argument
     setinverse <- function(inverse) inv <<-inverse
         
     # get the cached value
     getinverse <- function() inv
         
     # returns a list of all 4 functions
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }
 
   
   
 # The following function returns the inverse of the matrix. It first checks if
 # the inverse has already been computed. If so, it gets the result and skips the
 # computation. If not, it computes the inverse, sets the value in the cache via
 # setinverse function.
 # This function assumes that the matrix is always invertible.
  

 cacheSolve <- function(x, ...) {
         
     # get the cached value
     inv <- x$getinverse()
     # If a cached value exits, return it
     if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
     }
     # Otherwise get the matrix, calculate the inverse and store it in
     # cache
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
 }

