## The first function, makeCacheMatrix creates a special matrix object.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inc <<- NULL
    
  }
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv 
list(set =set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
  
}


## Next function, cacheSolve, computes the inverse of the special "matrix" created by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting chaced matrix")
    return(inv)
    
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}

## Reason why we use this type of functions is because matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.