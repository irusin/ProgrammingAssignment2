## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.

## Here are a pair of functions that cache the inverse of a matrix.


## First create a function which starts with a NULL matrix agrument
makeCacheMatrix <- function(x = matrix()) {
  ## start with the NULL value of the matrix
  minv <- NULL
  ## now declaring function set. Here we create the matrix for the first time and make
  ## changes to the cached matrix.
  set <- function(y) {
    x <<- y
    ## if the matrix was changed we change value of the cached matrix minv to NULL
    minv <<- NULL
  }
  ## get the value of the inverse
  get <- function() x
  ## Uses solve function to calculate the inverse of the matrix
  setinv <- function(solve) minv <<- solve
  ## getting the matrix inverse
  getinv <- function() minv
  ## passing the value of the makeCacheMatrix function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Using this function to cache the matrix
cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  ## if the inverse already exists, return this value and put a message
  if(!is.null(minv)) {
    message("getting cached data (inverse of the matrix)")
    return(minv)
  }
  ## if there are no inverse found, get data
  data <- x$get()
  ## calculate inverse matrix
  minv <- solve(data, ...)
  ## retrieve the result
  x$setinv(minv)
  ## return calculated matrix
  minv
}
