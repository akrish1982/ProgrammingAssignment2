## functions for Assignement week 3
## Two functions are a part of this file.
## 1. makeCacheMatrix 
## 2. cacheSolve

## makeCacheMatrix: matrix of functions to set/get values
## input is a matrix (example:  A <- matrix(c(5,1,0,3,-1,2,4,0,-1),nrow=3,byrow=TRUE)
## returns a list of 4 functions named set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y){
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invmatrix <<- inv
  getinv <- function() invmatrix
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## cacheSolve: retrieve inverse of matrix from cache if solution is available in cache
## if solution is not available it calls solve() function to solve
## input: type is a list as outputed in makeCacheMatrix()
## output: matrix (inverse of the original matrix used in makeCacheMatrix())

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinv()
  if(!is.null(invmatrix)) {
    message("getting cached matrix")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setinv(invmatrix)
  invmatrix
}

