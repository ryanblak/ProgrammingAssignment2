# An instance of makeCacheMatrix can be initialised by passing a 
# square matrix as an argument. The created instance can then be 
# passed to cacheSolve, which will invert the matrix, cache and 
# return  the result. Subsequent calls to cacheSolve with the 
# same makeCacheMatrix instance will return the cached result 
# and not invert the matrix again.

# Allows caching of a matrix and its inverse value
makeCacheMatrix <- function(matrix = matrix()) {
  inverseMatrix <- NULL
  
  # Caches a matrix and clears the inverse cache
  set <- function(newMatrix) {
    matrix <<- newMatrix
    inverseMatrix <<- NULL
  }
  
  # Gets the original unsolved matrix
  get <- function() matrix
  
  # Caches a solved matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  # Gets the solved matrix
  getInverse <- function() inverseMatrix
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


# Takes an initialised instance of makeCacheMatrix 
# and solves or accesses the cached matrix within it
cacheSolve <- function(matrixCacher, ...) {
  inverseMatrix <- matrixCacher$getInverse()
  
  # Check if the cached original matrix has been solved already
  if(!is.null(inverseMatrix)) {
    return(inverseMatrix)
  }
  
  # Get the original matrix
  matrix <- matrixCacher$get()
  
  # Solve the original matrix
  inverseMatrix <- solve(matrix)
  
  # Store the newly solved matrix
  matrixCacher$setInverse(inverseMatrix)
  
  # Return the newly solved matrix
  inverseMatrix
}
