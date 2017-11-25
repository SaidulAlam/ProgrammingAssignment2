# Matrix inversion is usually a costly computation.
# Caching the inverse of a matrix is better than compute it repeatedly. 
# The following two functions are used to cache the inverse of a matrix.


# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
# and creates a list containing a function to

# 1.  Set the value of the matrix
# 2.  Get the value of the matrix
# 3.  Set the value of inverse of the matrix
# 4.  Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
  
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) 
  inver <<- inverse
  
  getinverse <- function() inver
  
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)

}


# The cacheSolve function returns the inverse of the matrix. 
# At first it checks the inverse has already been computed. 
# If found,then it gets the result and skips the computation. 
# If not found, then it computes the inverse and sets the value in the cache 
# via setinverse function.

cacheSolve <- function(x, ...) {
  
  inver <- x$getinverse()
  
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
  
}
