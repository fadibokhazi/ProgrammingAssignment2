## Our aim in this experiment is to write a pair of functions, namely,
"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates our matrix object that can cache its inverse for the input 

makeCacheMatrix <- function(x = matrix()) {inv<- NULL
  set<- function(y){
      x<<- y
      inv<<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<-inverse}
  getInverse <- function() {inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## cacheSolve is a function which computes the inverse of the our matrix

cacheSolve <- function(x, ...) {
        inv <-x$getInverse()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
