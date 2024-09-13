makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set<- function(y) {
  x <<- y
  inv <<- NULL
  
  }
  get <- function(x)
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}
 
cacheSolve <- function(x, ...) {
   
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

makeCacheMatrix <- function(x = matrix()) {
  h <- NULL
  set <- function(y){
    x <<- y
    h <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) h <<- inverse
  getInverse <- function() h 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  h <- x$getInverse()
  if(!is.null(h)){
    message("getting cached data")
    return(h)
  }
  mat <- x$get()
  h <- solve(mat,...)
  x$setInverse(h)
  h
}


