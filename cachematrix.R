## This script provides a means for caching the inverse of a matrix.  This would provide
## a peformance boost when having to calculate the inverse of a matrix multiple times 
## for the same matrix.  
##
## Example use:
## > m=rbind(c(2, 4), c(1, 3))
## > o=makeCacheMatrix(m)
## > cacheSolve(o)
##       [,1] [,2]
## [1,]  1.5   -2
## [2,] -0.5    1
##

## makeCacheMatrix - Creates a special object that stores a numeric matrix and caches 
## its inverse.
## Expected parameter: x = matrix
## Methods: set(y) -- sets object's matrix value
##          get()  -- returns object's matrix value
##          setinverse(inverse) -- sets object's inverse cache to inverse paramete value
##          getinverse() -- returns cached inverse
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


## cacheSolve - This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.
## Expected parameters: 
##    x   = makeCacheMatrix object
##    ... = additional parameters to be passed in to solve() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
