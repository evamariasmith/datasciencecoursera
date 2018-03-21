## Programming Assignment 2 creates two functions that help you catch the inverse of a matrix
## because matrix inversion can be costly.

## This function creates a special "matrix" object that can cache its inverse.
## It does the following:
### sets the value of the matrix
### gets the value of the matrix
### sets the inverse of the matrix 
### gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cashed data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
