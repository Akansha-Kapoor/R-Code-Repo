## makeCacheMatrix is a function that creates an object that stores a 
## matrix and corresponding inverted matrix and four functions to manage 
## them.

## set(y) stores the matrix, and sets the inverse to NULL (since the
##          old value is invalid and needs to be recalculated)
## get() retrieves the matrix
## setmatrix(xinv) stores the inverted matrix
## getmatrix() retrieves the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<- function(y) {
    x <<- y
    m <<-NULL
  }
  get <- function() x
  setmatrix <- function(xinv) m <<- xinv
  getmatrix <- function() m
  list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## cacheSolve checks if there is a cached value and returns the value if 
##      available, otherwise recaculates the inverse and caches the new value


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if (!is.null(m)) {
    message("getting cached matrix...")
    return(m)
  }
  xm <- x$get()           ## xm gets the non-inverted matrix
  m <- solve(xm, ...)     ## m is the inverted matrix solution to be cached
  x$setmatrix(m)
}

## Running Example
## obj <- makeCacheMatrix()
##obj$set(matrix(sample(9),nrow=3,ncol=3))
##obj$get()
##> obj$get()
##[,1] [,2] [,3]
##[1,]    9    4    5
##[2,]    1    3    6
##[3,]    2    7    8
##> cacheSolve(obj)
##> obj$getmatrix()
##[,1]       [,2]        [,3]
##[1,]  0.127659574 -0.0212766 -0.06382979
##[2,] -0.028368794 -0.4397163  0.34751773
##[3,] -0.007092199  0.3900709 -0.16312057
##> cacheSolve(obj)
##getting cached matrix...
##[,1]       [,2]        [,3]
##[1,]  0.127659574 -0.0212766 -0.06382979
##[2,] -0.028368794 -0.4397163  0.34751773
##[3,] -0.007092199  0.3900709 -0.16312057