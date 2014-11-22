## this function allows us to cache an inverted matrix

makeCacheMatrix <- function(x = matrix()){  
  m <- NULL
  
  ## here's where we'll start to set values
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ##these are the methods within this function that can be called
  
  ## this returns the object (matrix)
  get <- function() { x }
  
  ##the set allows us to overwrite the inverse that was already calculated
  setinverse <- function(solve) { m <<- solve }
  
  ##allows us to retrieve the inverse calculated
  getinverse <- function() { m }
  
  ## makes the methods we've defined in this function accessible from other functions
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


##this function allows us to calculate the inverse of a matrix, leveraging cache

cacheSolve <- function(x, ...) {
  
  ## calculate the inverse by calling the function set up in makeCacheMatrix
  m <- x$getinverse()
  
  ## check to see if we've already calculated this and values are stored in the cache
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  
  ## if we haven't already calculated the inverse, here we will do that
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## here we will output the result to the screen
  m
  
}