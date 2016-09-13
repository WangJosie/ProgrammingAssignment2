## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #set the value of matirx
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    #return the value of matrix
    get <- function() x
    
    #set(cache) the value of the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
    
    #return the value of the inverse
    getinverse<- function() m
    
    #return this "matrix"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  #if the inverse has already been calculated and cached, return it
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  data <- x$get()
  #otherwise, get the value using solve
  m <- solve(data,...)
  x$setinverse(m)
  m
}