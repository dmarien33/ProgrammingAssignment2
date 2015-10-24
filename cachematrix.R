##   makeCacheMatrix makes a list of four functions:
##   getmyM returns the square, invertible matrix stored in the function makeCacheMatrix
##   setmyM changes the matrix stored in the function makeCacheMatrix and returned by getmyM
##   setIM stores an inverted matrix in the function makeCacheMatrix
##   getIM gets the inverted matrix stored in setIM 

## functions setmyM and setIM should only be used by the function
## if they are changed directly by the user, the results may be misleading

makeCacheMatrix <- function(x = matrix()) {
    IM <- NULL
    setmyM <- function(y) {
    x <<- y
    IM <<- NULL
  }
  getmyM <- function() x
  setIM <- function(invM) IM <<- invM
  getIM <- function() IM
  list(setmyM = setmyM, getmyM = getmyM,
       setIM = setIM,
       getIM = getIM)
}

## cacheSolve calculates the inverse of the matrix stored in setmyM by makeCacheMatrix
## it gets the matrix by using getmyM in list x, and, if getIM in x is null,
## it calculates the inverse by using solve() and stores it in setIM
## if getIM in x is not null, it returns the inverted matrix stored in getIM 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  sol <- x$getIM()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  data <- x$getmyM()
  sol <- solve(data)
  x$setIM(sol)
  sol  
  
}