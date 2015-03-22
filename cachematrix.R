## Programming Assignment 2: Lexical Scoping
## These functions create a square Matrix, and will then calculate the inverse and then store it.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Store the cache, it is initially null
  cache <- NULL
  
  ## set (create) the matrix
  set <- function(y) {
    x <<- y
    ## new matrix, so we don't know the inverse yet, set to NULL again.
    cache <- NULL
  }
  ## Show the matrix
  get <- function() x
  
  ##set the inverse of the matrix
  setinverse <- function(inv) cache <<- inv
  
  ##Get the inverse of the matrix
  getinverse <- function() cache
  
  #the functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##is the inverse calculated yet? I is inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting inverse data")
    return(i)
  }
  message("calculating inverse data")
  ## get the matrix
  data <- x$get()
  
  ## Sovle the data
  i <- solve(data)
  
  x$setinverse(i)
  
  i
}