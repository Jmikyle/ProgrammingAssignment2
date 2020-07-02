makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) inv <<- inverse
  getinverse <- function(){inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting chached data")
    return(inv)
  }
  data <- inv$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}