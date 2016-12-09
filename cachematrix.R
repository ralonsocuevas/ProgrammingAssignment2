## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" that can save it´s inverse in the cache, it contains the functions 
##set, get, setinversa and getinversa

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) 
    {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinversa <- function(solve) inversa <<- solve
  getinversa <- function() inversa
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}


## This function calculates the inverse of a matrix, first it searches if the same matrix has been calculated and saved in 
## cache (if the answer is yes, it retrieves it from the cache), if not, it calculates the inverse and save it with the makeCacheMatrix

cacheSolve <- function(x, ...) {
  inversa <- x$getinversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinversa(inversa)
  inversa
}
