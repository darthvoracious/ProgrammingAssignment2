## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverse = NULL
  set = function (value) {
    x <<- value
    inverse <<- NULL
  }
  
  get = function() x
  setinverse = function(value) inverse <<- value
  getinverse = function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inverse = x$getinverse()
        
        if (!is.null(inverse)){
          # get it from the cache
          message("Get Cached Data")
          return(inverse)
        }
        
        mat.data = x$get()
        inverse = solve(mat.data, ...)
        
        x$setinverse(inverse)
        
        return(inverse)
}
