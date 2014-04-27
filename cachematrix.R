## This function creates a special object "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m  <- NULL
  set  <- function(y){
    x <<- y
    m <<- NULL 
  
  }
  get  <- function() x
  setinverse  <- function(inverse) m  <<- inverse
  getinverse  <- function() m
  
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

## This function calculates the inverse of the "matrix" 
## special makeCacheMatrix returned by above. 
## If the reverse is already calculated (and the matrix has not changed), 
## then the reverse cachesolve must retrieve cache.

cacheSolve <- function(x, ...) {
  
  m  <- x$getinverse()
  if (!is.null(m)){
    message("waiting for cache data")
    return(m)
  }
  
  data  <- x$get()
  m  <- solve(data, ...)
  x$setinverse(m)
  m
}

## This coding allows us to create a matrix of order nxn, 
## which also has random numbers created by the rnorm function, 
## this matrix will be presented in the form of its inverse by 
## applying the above coding performed.
## the mean and standard deviation are chosen according to what 
## the user wants to perform calculations

r <- 100
c <- 100
mtx <- makeCacheMatrix(matrix(rnorm((r*c),mean=1.8,sd=0.13),nrow=r,ncol=c))
cacheSolve(mtx) 

