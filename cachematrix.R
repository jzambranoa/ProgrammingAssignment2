# This function creates a matrix, which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
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

# This function returns the inverse of the matrix. It first checks if
# the inverse of the matrix has already been computed. If so, it gets the result and 
# skips the computation. If not, it computes the inverse, sets the value in the cache via
# setInverse function.
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

#Example
#x = matrix(1:4,2,2)
#m = makeCacheMatrix(x)
#m$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#cacheSolve(m)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#cacheSolve(m)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
