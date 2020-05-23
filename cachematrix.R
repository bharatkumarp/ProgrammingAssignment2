#makeCacheMatrix and cacheSolve together used in caching matrix inverse
#Usage:
#B <- matrix( c(5, 1, 0,
#                   3,-1, 2,
#                   4, 0,-1), nrow=3, byrow=TRUE)
#y<-makeCacheMatrix(B)
#cacheSolve(y)


## makeCacheMatrix stores functions in list and returns the list

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    mat_in <<- y
    inv_mat <<- NULL
  }
  get <- function() mat_in
  setinv <- function(inv_mat_inp) inv_mat <<- inv_mat_inp
  getinv <- function() inv_mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve looks for inv_mat 
# If inv_mat is null, then it gets matrix from object
# computes Inverse of matrix and caches the matrix inverse first timee
# Further invocations will return cached matrix inverse

cacheSolve <- function(x, ...) {
  inv_mat <- x$getinv()
  if (!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data)
  x$setinv(inv_mat)
  inv_mat
}
