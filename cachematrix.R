# Assignment: Caching the Inverse of a Matrix
#
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. This pair of functions cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed),
## then `cacheSolve` should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

# tests
iterations <- 100000
sampleMatrix <- rbind(c(1, -1/4), c(-1/4, 1))
expectedResult <- solve(sampleMatrix)

withoutCache <- (function(matrix, iterations){
  print("testing non cached version...")
  t0 <- Sys.time()
  for(x in 1:iterations)
    solve(sampleMatrix);
  Sys.time() - t0
})(sampleMatrix, iterations)

withCache <- (function(matrix, iterations){
  print("testing cached version...")
  cachedMatrix <- makeCacheMatrix(sampleMatrix)
  t0 <- Sys.time()
  for(x in 1:iterations)
    cacheSolve(cachedMatrix);
  Sys.time() - t0  
})(sampleMatrix, iterations)

print("test succeeded?")
(withCache < withoutCache) && (expectedResult == cacheSolve(cachedMatrix))
