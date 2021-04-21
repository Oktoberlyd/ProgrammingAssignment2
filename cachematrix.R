####################################################
##                                                ##
##  R Programming Assignment 2                    ##
##  Lexical Scoping and Functions                 ##
##                                                ##
##                                                ##
####################################################

## Two functions are created
## makeCacheMatrix() 
## - function that creates an R object that stores a vector and its inverse matrix
## cacheSolve()
## - function that either calculate the inverse matrix or retrieves it from the stored makeCacheMatrix environment


## makeCacheMatrix() builds a four functions and returns the functions within a list in the parent environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## by applying the object created in makeCAcheMatrix to the cacheSolve() function 
## then cacheSolve() is able to check if
## the object contains the inverse matrix an either retrieve it 
## or if not contained calculate it and store it in the parent environment

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


#Example:
A_matrix <- matrix( 
  c(-2, 3, 5, 7), 
  nrow=2, 
  ncol=2)

testA <- makeCacheMatrix(A_matrix)
cacheSolve(testA)

#Another example:
B_matrix <- matrix(
  c(1001, -22, 42, 9749, 13, 2007, 391, 400221, 11),
  nrow = 3,
  ncol = 3)

testB <- makeCacheMatrix(B_matrix)
cacheSolve(testB)
