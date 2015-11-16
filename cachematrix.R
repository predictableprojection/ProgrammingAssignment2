


## Function makeCacheMatrix creates a matrix object, consisting of a set of 
## four functions for storing a matrix and its inverse.
## Works in conjunction with function cacheSolve, below.


makeCacheMatrix <- function(x = matrix()) {
  ## Creates a matrix object x, consisting of a set of 
  ## four functions for storing a matrix and its inverse.
  ## Returns a list of functions:
  ## x$setmat(A) sets matrix equal to A
  ## x$getmat() returns matrix
  ## x$setinv(B) sets inverse equal to B (Use function cacheSolve for this.)
  ## x$getinv() returns inverse of matrix
  
 invmat <- NULL
  setmat <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  getmat <- function() x
  setinv <- function(inv) invmat <<- inv
  getinv <- function() invmat
  return(list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv))

}



#  ------------------------------------------------------------------------


## Function cacheSolve will return the inverse a matrix created using 
## makeCacheMatrix. If the inverse is already stored in cache, it will call 
## it from there. 
## If not, it will compute it.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of matrix object x, created using 
  ## function makeCacheMatrix
  invmat<-x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }else{
    mat <- x$getmat()
    invmat <- solve(mat,...)
    x$setinv(invmat)
    return(invmat)
  }
}


