## Functions that cache the inverse of a matrix for assignment 2


## Create a matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Start the inverse property
  i <- NULL
  
  ## To set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## To get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## To set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## To get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix"
## If the inverse has been calculated 
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse using matrix
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}

##end of assignment2