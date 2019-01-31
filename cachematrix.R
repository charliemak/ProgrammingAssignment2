## cacheSolve() function performs matrix inversion on makeCacheMatrix() matrix object and  
## either establishes the computation in memory or, if already performed against same object, 
## retrieves the cached computation

## Establishes list of set and retrieve functions to initialize makeCacheMatrix() matrix object 
## and perform downstream matrix inversion using 'cacheSolve' function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  get <- function() x 
  setinv <- function(inv) i <<- inv
  getinv <- function() i 
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Takes as an argument a matrix from makeCacheMatrix() object and either establishes matrix inversion 
## computation or, if previously done for same makeCacheMatrix() object, retrieves the cached computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("inversion previously performed on this matrix---retrieving cached computation")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i) 
  i
}
