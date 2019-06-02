
## this function to create a matrix and cache it.
# you need to assign it to a variable for example 
#matrix_cached <- makeCacheMatrix()
# then you must set the values of the matrix by the following
#matrix_cached$set(matrix(1:4, 2))

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the above matrix is now stored as matrix_cached
# use the below function in the following manner
# cacheSolve(matrix_cached)
# and the inverse of the matrix will be given

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}