## Put comments here that give an overall description of what your
## functions do


## This function creates a "matrix" object that can cashe its inverse.
## It stores a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## The set function changes the vector stored in the main function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get is a function that returns the vector x stored in the main function
  get <- function() x
  ##setinverse is a function that stores the value of the input in a variable m 
  setinverse <- function(inverse) m <<- inverse
  ##getinverse is a function that returns the valuve of m
  getinverse <- function() m
  
  ##The following line stores the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the "matrix" returned by makeCacheMatrix
## If the inverse has already been calcuated, 
##then this funtion will retireve the inverse of the matrix from the cache.

cacheSolve <- function(x) {
        ## The folowing gets the inverse tos ee if it has been computed
  m <- x$getinverse()
  ##If it has then it will tell you and withll return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##If it the inverse hasn't already been computed it will get the matrix and then..
  data <- x$get()
  ##...the following finds the inverse
  m <- solve(data)
  ##The following caches the result
  x$setinverse(m)
  m
}
