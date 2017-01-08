## There are two functions here given below
## Since matrix inversion is a lengthy process, the objective here is to cache the inverse of a matrix
## Thi will allow us to skip the computation if the inverse already exists

## The function below here cahce the inverse of the "matrix"

makeCacheMatrix <- function(x = matrix()) {
  
     INVSE <- NULL
    set <- function(y) {
      x <<- y
      INVSE <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) INVSE <<- inverse
    getInverse <- function() INVSE
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
  

## Here we compute the actual inverse. Also, if the latter exists, it would simply get it from the cache generated from above

cacheSolve <- function(x, ...) {
  
  INVSE <- x$getInverse ()
  
  if(!is.null(INVSE)) {
    message("getting cached data")
    return(INVSE)
  }
  data <- x$get()
  INVSE <- solve (data, ...)
  x$setInverse(INVSE)
  INVSE
        
}
