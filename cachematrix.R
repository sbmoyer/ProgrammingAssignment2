## This function creates a special "matrix" object that
## can cache its inverse

## The function creates an R object that introduces a
## a variable "s" to save the inverse matrix, creates get()
## function to get the matrix, then uses setMatrix() to assign
## the inverse matrix to s.  getMatrix() gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  get <- function() x
  setMatrix <- function(Matrix) s <<- Matrix
  getMatrix <- function() s
  
  ##return a list
  list(get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## This function finds the inverse of the matrix.  The function
## searches the cache to see if the inverse matrix is there
## if the matrix is there, the function returns the matrix.
## If the matrixis not there the function returns the inverse
## of the x matrix

cacheSolve <- function(x) {
  s <- x$getMatrix()
  if(!is.null(s)){
    return(s)
  }
  else {
    data <- x$get()
    s <- solve(data)
    x$setMatrix(s)
    return(s)
  }
}
