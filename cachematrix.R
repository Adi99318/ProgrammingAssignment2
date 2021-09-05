## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL             #initialise inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  #to define the function to get matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) # to get cache data 
  {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) #function to return the inverse value
  }
  data <- x$get()
  inv <- solve(data, ...) #calculate inverse value
  x$setinv(inv) #set the value of inverse
  inv      ## Return a matrix that is the inverse of 'x'
}
