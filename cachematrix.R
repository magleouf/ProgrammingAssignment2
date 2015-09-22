##This file consits of two functions: "makeCacheMatrix" and "cacheSolve".
##
##1)The function "makeCacheMatrix" takes a matrix as input, and returns a list
##  of four subfunctions as output: 
##	a)The subfunction "get" simply return the input;
##	b)The subfunction "set" modifies the input stored in the main function;
##	c)The subfunction "setinv# assigns a value to the matrix inverse; 
##	d)The subfunction "getinv" returns the stored value of the inverse of 
##	  the stored matrix;
##
##2)The function "cacheSolve" computes the inverse of a (square and invertible)
##  matrix, after checking whether this inverse is already chached.


## The function "makeCacheMatrix" is defined below. 

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invmat <<- inverse
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function "cacheSolve" is defined below. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}

