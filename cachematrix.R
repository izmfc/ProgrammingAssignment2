

## The first function create a special oject "Matrix"  that
## stores a matrix and cache's its inverse.
## The second one calculates the inverse of the special 
## "matrix" created in the first function. However, it 
## first checks if the inverse of the matrix has already
## been calculated. Otherwise, it calculates the inverse of 
## the matrix
 

## MakeCacheMatrix: stores a matrix and cache's its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: read or calculate the inverse of a special "matrix"
## Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
