## Programming Assignment 2 for R Programming on Coursera
## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y){
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) invx <<- inv
        getinverse <- function() invx
        list(set = set,get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated, then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invx <- x$getinverse()
  if(!is.null(invx)){
    message("getting cached inverse")
    return(invx)
  }
  Data <- x$get()
  invx <- solve(Data)
  x$setinverse(invx)
  invx
}
