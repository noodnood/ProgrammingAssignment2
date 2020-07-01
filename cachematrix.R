## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##

makeCacheMatrix <- function(x = matrix()){
  output <- NULL
  set <- function(y){
    x <<- y
    output <<- NULL
  }
  get <- function(){x}
  setInv <- function(inverse){output <<- inverse}
  getInv <- function(){m}
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  output <- x$getInv()
  if(!is.null(output)){
    message("getting cached data")
    return(output)
  }
  
  data <- x$get()
  output <- solve(data, ...)
  x$setInv(output)
  output
}
