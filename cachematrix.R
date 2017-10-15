
## Create an object that calculates the inverse of a matrix the first 
## time you need and then uses the cached results each time after that

## generates the object that will store the matrix inverse.  Note that no 
## calculations are done in this function and inverse won't be calculated 
## until the first time you try to get it 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Get the matrix inverse of a makeCacheMatrix object.  The inverse will 
## only be calculated the first time you call the function.  On each 
## following call a cached result will be retuned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
