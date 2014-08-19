## makeCacheMatrix creates a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the inverseMatrix
## get the value of the inverseMatrix
makeCacheMatrix <- function(x = matrix()) {
  message("called makeCacheMatrix")
  i <- NULL
  set <- function(y) {
    x <<- y
    ## resetting i, the inversed Matrix
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse){
    message("called makeCacheMatrix.setsolve")
    i <<- inverse
  } 
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve returns a matrix that is the inverse of 'x' - x needs to be a square matrix
## if the inverse has previously been calculated, the value is returned from the cache
## otherwise it calculates the inverse of x and sets the value of inverse in the cache via setsolve()
cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
  ## calculate Inverse of the matrix
  i <- solve(data, ...)
  x$setsolve(i)
  i
}