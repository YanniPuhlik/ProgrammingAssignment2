## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inverse<-NULL
    set<-function(y){
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse<- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

Example
x = matrix(1:4,2,2)
m = makeCacheMatrix(x)
m$get()

## [,1] [,2]
## [1,]    1    3
## [2,]    2    4




## Write a short comment describing this function
##  This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve (data)
  x$setinverse(inv)
  inv
}

Example 
x = matrix(1:4,2,2)
cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

