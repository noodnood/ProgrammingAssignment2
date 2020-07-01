## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##following the templete, I changed x to be a matrix
##set function sets the value of the matrix
##get function gets the value of the matrix
##setInv function sets the value of the inverse matrix
##getInv function gets the value of the inverse matrix

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
##if it hasn't, it computes the inverse and sets the value of the inverse via setInv()

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
