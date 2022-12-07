## These functions are will create a special object that will store a matrix and cache
## (save in global memory) the inverse of said matrix to increase the speed at which we
## can solve the matrix with the latter function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes in the special object created by the function above. It checks to see if the
## inverse of the matrix already exists and if the original matrix has been modified or not. If both these
## are true, then it retrieves the inverse of the matrix to solve it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


# Creating a 3 X 3 Matrix
M <- matrix(c(7,5,2,5,3,6,9,1,3),3,3)

# Testing the makeCacheMatrix functions
M1 <- makeCacheMatrix(M)
cacheSolve(M1)

# Testing the cacheSolve functions
cacheSolve(M1)
