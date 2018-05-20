## The first function, makeMatrix creates a special "matrix", which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    setinv the value of the inverse
##    getinv the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
}
    get <- function() x
    setinv <- function(inv) m <<-inv
    getinv <- function() m
    list(set =  set, get = get, setinv = setinv, getinv = getinv)
}

## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
}

#Exemple:
#> source ("cachematrix.R")
#> inv <-makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
#> cacheSolve (inv)
#[,1] [,2]
#[1,]  0.5  0.0
#[2,]  0.0  0.5
