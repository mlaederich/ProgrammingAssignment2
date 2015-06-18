## Put comments here that give an overall description of what your
## functions do


## This function creates a "matrix" objects that can cashe its inverse.
## It stores a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get is a function that returns the vector x stored in the main function
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  ##The following line stores the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This funciton computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calcuated, 
##then this funtion will retireve the inverse of the matrix from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
