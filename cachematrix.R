## Author: Alexander Schniertshauer
##
## Date: 2014-09-20
##
## Version: 1.0
##
## This script is the result of my work on the second programming assignment of 
## Coursera's R-Programming course. The assignment requires to write two R 
## functions that are able to cache matrix inversion - a potentially costly 
## computation.The script takes advantage of the scoping rules of the R
## language and how they can be manipulated to preserve state inside of an R
## object.
## 
## The script is by and large built on top of the example provided in the course
## (calculating and caching the mean of a vector)
##
## ------------------------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.In a nutshell the function creates a special "matrix",
## which is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix 
## 3.set the value of the inverse 
## 4.get the value of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
##
## -----------------------------------------------------------------------------
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve will retrieve the
## inverse from the cache.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
##