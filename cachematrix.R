##These two functions are created to calculate the inverse of a matrix and store its caches. The other purpose of these functions are to shorten the process of matrix inversion by caching the inverse of the matric instead of having it to compute repeatedly.

##This first function will create a matrix that can store cache of its own result.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This second function will give us the inverse of the result that was given by the previous function, makeCacheMatrix. If by chance this function has already been executed and the inverse was calculated with same matrix, then the result of this function will be retrieved from the cached data instead.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}