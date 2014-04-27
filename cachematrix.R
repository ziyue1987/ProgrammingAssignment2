## By using these two functions, you can get a matrix with its's cached inverse.
## The matrix's inverse is computed when you call cacheSolve on it first time.
## Next time, if the matrix is not changed, the inverse can just be retrieved from cache.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # The new matrix data can be set by this function
  # It also means the cached inverse is invalid now
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # The matrix data can be get by this function
  get <- function() x
  
  # The inverse can be cathed by this function after it is computed first time.
  setinverse <- function(inverse) i <<- inverse
  
  # If the inverse was computed, this function will return it immediately, or it will return NULL
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # If the inverse was cached, just get it.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  # If the inverse was not computed, compute and cache it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
