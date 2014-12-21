## makeCacheMatrix function creates an object that stores a matrix and its caches it's inverse
##

## makeCacheMatrix creates a list which contains the following functions:
## 1. set: to set the value of the matrix x
## 2. get: to get the value of the matrix x
## 3. setinv: to set the inverse of the matirx x if the inverse is not already cached
## 4. getinv: to get the cached value of inverse of the matric x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse){
    inv <<- inverse
  }
  
  getinv <- function() inv
  
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve is a function that returns the cached value of the inverse of a matrix.
## If the inverse if the matrix is not found then it calculates and caches the inverse and returns the value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}