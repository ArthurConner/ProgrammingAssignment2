## cachematrix
## This creates a special matrix object that stores
## the cached value of an inverse

## This function returns the special matrix object
## that has its own protected property of inverse
## The cache solve makes use of these functions
## to store an inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv<<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This returns a matrix that is the inverse of our
## special matrix x
## it stores so we don't have to recompute the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setinverse(inv)
  inv
}
