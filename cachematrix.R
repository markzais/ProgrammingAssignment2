## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object that can cache a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set <- function(y){
    x <<- (y)
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get, seti = seti, geti = geti)
}


## Write a short comment describing this function
## This function solves the inverse of a matrix.  
## If the inverse has already been solved, it is retreived from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i = x$geti()

  # if the inverse has already been calculated
  if (!is.null(i)){
    # get it from the cache and skips the computation.
    message("Stand by. Getting cached data")
    return(i)
  }

  # otherwise, calculates the inverse
  mat.data = x$get()
  i = solve(mat.data, ...)

  # sets the value of the inverse in the cache via the setinv function.
  x$seti(i)

  return(i)
}
