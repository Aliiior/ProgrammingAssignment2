
#we were suppose to write two functions, with a name
#"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
#Write a short comment describing this function
#makeCacheMatrix is a function which creates a special "matrix" object that can
#cache its inverse for the input (which is an invertible square matrix)

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


#cacheSolve is a function which computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the
#inverse from the cache

cacheSolve <- function(x, ...) {

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



#caching the invers of matrix:
#Matrix inversion is usually a costly computation and there may be some
#benefit to caching the inverse of a matrix rather than compute it repeatedly.
#Below are a pair of functions that are used to create a special object that
#stores a matrix and caches its inverse.
#This function creates a special "matrix" object that can cache its inverse.


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


#This function computes the inverse of the special "matrix" created by
#makeCacheMatrix above. If the inverse has already been calculated (and the
#matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

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