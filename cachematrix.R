# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly 

## creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){ 
    x <<- y 
    i <<- NULL
  }
  get <- function() x
  setInv <-function(inv) i <<- inv
  getInv <- function() i
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


#The following function calculates the inverse of the special "matrix"
#created with the above function. However, it first checks to see 
#if the inverse has already been calculated. If so, it gets the inverse 
#from the cache and skips the computation. Otherwise, it calculates 
#the inverse of the matrix and sets the value of the inverse in the cache 
#via the setInv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  c <- x$getInv()
  if(!is.null(c)){
    message("Getting from Cache")
    return (c)
  }
  m<-x$get()
  d<-solve(m,...)
  x$setInv(d)
  d
}
