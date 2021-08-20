## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){ #creates the special matrix that can cache its inverse
  inv <- NULL 
  set <- function(y){ #set function 
    x<<-y
    inv <<- NULL
  }
  get <-function() {x} # get function 
  setInverse <- function(inverse) {inv <<- inverse} # set the value of inverse
  getInverse <- function() {inv} #get the value of inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
  
  #parent function is the makecachefunction 
  #double arrows is capable of being applied to parent function as well unlike one arrow which applies to current level 
cacheSolve <- function (x, ...){ #computes the inverseof the special "matrix" returned by makeCacheMatrix above
  inv <-x$getInverse() #line returns a matrix that is inverse of X and assigns it to inv 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <-x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
  
## Write a short comment describing this function

