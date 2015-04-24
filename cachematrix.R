# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of inverse of the matrix
#4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) m_inv <<- inverse
  
  getInverse <- function() m_inv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m_inv <- x$getInverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data)
  x$setInverse(m_inv)
  m_inv
}
