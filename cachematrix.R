## This pair of functions is used to save time while reversing large matrices (using cache)

## Remember, that the matrix passed as argument must be invertible and square - I don't want to
## import any libraries now. In other cases function will raise errors:
## 1) Lapack routine dgesv: system is exactly singular
## 2) Error in solve.default(data, ...) : 'a' (ncol x nrow) must be square
## I tested this using matrix(rnorm(), ncol = 4, nrow = 4) in order to see if all works.

## This function caches the reverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function returns cached reverse if exist, else caches

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}