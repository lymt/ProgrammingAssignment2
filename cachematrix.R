#' Create a matrix that will cache its inverse
#'
#' Takes a matrix and caches its inverse. Assumes x is invertible
#'
#' @param x a matrix, numeric only
#'
#' @return a list, of getters and setters for x and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get <- function() x

  setInverse <- function(invmat) inv <<- invmat
  getInverse <- function() inv

  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


#' Solve the inverse of a matrix using a cache
#'
#' Returns the inverse of x, checking the cache first. Creates a new
#' cache if none exists yet.
#'
#' @param x a matrix, numeric only
#' @param ... optional args passed to solve()
#'
#' @return a matrix, inverse of x
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
