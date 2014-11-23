## makeCacheMatrix() substantiates an object in cache which can hold a matrix and it's inverse
## It can be intialized without a matrix.   x <- makeCacheMatrix() or x <- makeCacheMatrix('a matrix')
##
## Other methods in in the object are
##
## 1) x$get() will return the matrix stored in the object
## 2) x$set('a matrix') will set the matrix in the object
## 3) x$getinv()  will return the inverse
## 4) x$setinv('inverse matrix') will set the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverseM) inv <<- inverseM
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve can calculate the inverse of the stored matrix in the object substantiated above
## and store the inverse within the object
## upon execution it will either return the stored inverse or if none stored ( or matrix has changed ) 
## it will calculate a new inverse and return new inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
