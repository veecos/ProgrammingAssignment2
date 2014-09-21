## The makeCacheMatrix functions caches the value of the inverse of the matrix, which is computed using the solve function
## As mentioned in the assignment this only works for "invertible matrices" 

## Description of the Function
## We use 4 functions 1. set - For initialization 2. get - to get the matrix 3. setinv - for finding the inverse 4. getinv - to return the cached value of inverse
## The set function is used for initializing and also for resetting when switching to other data sets
## The get function is basically used to read the data to another variable in the second function
## The setinv uses the solve function to find the inverse of the matrix
## The getinv returns the value if its already present

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL

  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  
  list(set = set, get = get,setinv = setinv,getinv = getinv)

}


## Description of the Function
## The function initially checks if there is a cached value of the inverse and returns it if present while printing " Getting Cached Data "
## if there is no cached value then the matrix is passed, the data is obtained via get(), the inverse computed via solve() and stored in the cache via setinv()

cacheSolve <- function(x, ...) {
  
  i <- x$getinv()
  
  if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setinv(i)
  
  i
}
