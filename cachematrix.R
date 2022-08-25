## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function(){
                           inver<-ginv(x)
                           inver%*%x      #function to obtain inverse of the matrix
                          } 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##this is used to get the cache data

cacheSolve <- function(x, ...) 
  ## Return a matrix that is the inverse of 'x'
{
  inv <- x$getinverse()
  if(!is.null(inv)) {             #checking whether inverse is null
    message("getting cached matrix inverse")
    return(inv)       #returns inverse value 
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv        ##Return a matrix that is the inverse of 'x'
}

