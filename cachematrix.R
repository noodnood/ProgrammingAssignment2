## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##following the templete, I changed x to be a matrix
##set() assigns the input arg to the x object & NULL to inv object in parent env
##get() retrives x object from parent env
##setInv() as a setter, sets input arg 'inverse' to value of inv in parent env
##getInv() as the getter, gets value of inv set in setInv()

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInv <- function(inverse){inv <<- inverse}
  getInv <- function(){inv}
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
##cacheSolve() checks if the inverse has already been computed 
##if it hasn't, it computes the inverse using solve() and sets the value of the inverse via setInv()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
