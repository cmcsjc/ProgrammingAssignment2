makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   ## Set m as null
  
  set <- function(y) {   ## Set x as y, and m as null
    x <<- y
    m <<- NULL
  }
  
  get <- function() x  ## Get matrix
  
  setInverse <- function(inverse) m <<- inverse  ## Store inverse into m
  
  getInverse <- function() m   ## Get the inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}