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
(function(){
  
  iterations <- 100000
  sampleMatrix <- rbind(c(1, -1/4), c(-1/4, 1))
  expectedResult <- solve(sampleMatrix)
  
  print("testing non cached version...")
  t0 <- Sys.time()
  for(x in 1:iterations)
    solve(sampleMatrix);
  durationWithoutCache <- Sys.time() - t0
  print(c("=> without cache:", durationWithoutCache))
  
  print("testing cached version...")
  cachedMatrix <- makeCacheMatrix(sampleMatrix)
  t0 <- Sys.time()
  for(x in 1:iterations)
    cacheSolve(cachedMatrix);
  durationWithCache <- Sys.time() - t0
  print(c("=> with cache:", durationWithCache))
    
  faster <- durationWithCache < durationWithoutCache
  sameRes <- all.equal(expectedResult, cacheSolve(cachedMatrix))
  
  print(c("test: faster with cache?", faster))
  print(c("test: same result?", sameRes))
  print(c("test result:", faster && sameRes))
  
  faster && sameRes
})()
