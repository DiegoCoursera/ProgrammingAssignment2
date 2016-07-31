## Having an invertible matrix, it can be use the following  2¡two functions that will calculate the inverse matrix or retrieve the inverse matrix from the cache.
## The Function “makeCacheMatrix” creates a “matrix” That can catche the inverse of the same "Matrix".


## 1) set is a function that changes the vector stored in the main function.
##2) get is a function that returns the vector x stored in the main function.


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The function CacheSolve its bringing the inverse of the matrix coming from the makeCacheMatrix.
## if the case its that the inverse has already been calculated and the matrix its the same, then the cachesolve
## should retrieve the inverse from cache. if the inverse its not yet calculated, then data gets the matrix stored with MakeCacheMatrix
## With m Caclculating the inverse and x$setmean(m) storing it in the object m in makeCacheMatrix. 


cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
