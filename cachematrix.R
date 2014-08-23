## Calculate the inverse of a matrix. 
##It is done just once, checking if  the result is already in the cache. 


## Creates a list containing four functions to set(), get() the matrix
## to getinv() and setinv() to find out it the inverse was 
## calculated before

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  } 
  get <- function() x 
  setinv <- function(mean) m <<- mean 
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv, 
       getinv = getinv)
  
}



##Every time the inverse of hte matrix is needed the cache is checked up. 
##If the inverse was calculated before it is brought from the cache 
##If the inverse wos not calculated, this function does it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  } 
  data <- x$get() 
  m <- solve(data, ...) 
  x$setinv(m) 
  m
}
