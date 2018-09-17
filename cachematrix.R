## this program is to understand two functions 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Returning a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## for executing the code enter following code in console
## m <- matrix(rnorm(9),3,3)
## cacheSolve(m1)
## this result will be obtained
##            [,1]        [,2]       [,3]
##[1,]  0.7417821  -0.3462817 -0.3979721
##[2,] -8.0532684 -10.6008395  7.2558793
##[3,] -0.7368461  -1.1623834 -0.9073427
## you can also use m <-matrix(rnorm(16),4,4) for 4x4 matrix
