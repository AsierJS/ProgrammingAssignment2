## The first function, makeMatrix creates a special "matrix", which is really a list containing a function to
##    set the value of the vecctor
##    get the value of the vector
##    set the value of the inverse
##    get the value of the inverse


## Write a short comment describing this function

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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
