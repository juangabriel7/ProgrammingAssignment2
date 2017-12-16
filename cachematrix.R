## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a general function to set and get the data and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## Write a short comment describing this function
## simple if function to determine if there is an existing data already in Cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  y <- makeCacheMatrix(x)
  i <- y$getinverse()
  if(!is.null(i)) {
    
    message("getting cached data")
    return(i)
  }
  
  
  data <- y$get()
  i <- solve(data, ...)
  y$setinverse(i)
  i
  
}
