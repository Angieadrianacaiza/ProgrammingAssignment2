## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This pattern is often used to improve the performance of functions that repeatedly calculate the inverse of the same or similar matrices.
##Function to create an object that stores an array and its inverse (if computed)
# Uses a mechanism of cache to avoid recalculating the inverse unnecessarily.


makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL           ##initialize the inverse property
  ##method to set the matrix
  set <- function(y){       ##sets the value of the matrix
    x <<- y
    inversa <<- NULL     ##reset the inverse property
  }
  get <- function(){x}       ##retrieves the value of the matrix
  setInversa<- function(inversacalculada) {inversa <<- inversacalculada}  ##sets the value of the inverse of the matrix
  getInversa <- function(){inversa}             ## retrieves the value of the inverse of the matrix
  ##return a list of the methods
  list(set = set, get = get, 
       setInversa = setInversa, 
       getInversa = getInversa)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" created by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then it retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
  inversa <- x$getInversa()     ##retrieves the inversa if it is already caches
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()              ##get the matrix
  inversa <- solve(data,...) ##calculate the inverse 
  x$setInversa(inversa)      ##cache the inverse
  inversa                     ##return the inversa
}
