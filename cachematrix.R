## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Cache the matrix!

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



## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse   ## Get inverse of x and store in m
  
  if(!is.null(m)) {   ## check if m isn't null
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()    ## store x into data
  
  m <- solve(data, ...)    ## solve for inverse and store in m
  
  x$setInverse(m)  ## Set x to m
  
  m  ## return m
}
 