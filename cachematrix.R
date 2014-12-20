## Put comments here that give an overall description of what your
## functions do
## the CashSolve() function takes a list as argument that contain the set, get, setinverse, getinverse 
## as elements and if the values are not null(i.e., if the values cached) will return without 
## computing the inverse of matrix using R function Solve(). If the list is null, then it computes
## the inverse and assignes(caches) the value computed value

## Write a short comment describing this function

## makeCacheMatrix () : This function caches the input matrix, returns a list of set, get, setinverse, getinverse 
## AND this works for 2x2 invertible matrix 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    assign(x, y, inherits = TRUE)
    assign(m, NULL, inherits = TRUE)
  }
  get <- function() x
  setinverse <- function(inverse){
    m <<- inverse
    ##assign(m, inverse, inherits = TRUE)
  } 
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function
## returns the cached inverse matrix OR, if the inverse is computed for the first time, comptes the inverse, assigns the value and returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
